/*
 * Copyright 2014 Mike Hearn
 * Copyright 2014 Andreas Schildbach
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bitcoinj.wallet;

import com.google.common.collect.*;
import com.google.protobuf.*;

import org.bitcoinj.core.*;
import org.bitcoinj.crypto.*;
import org.bitcoinj.script.*;
import org.bitcoinj.script.Script.ScriptType;
import org.bitcoinj.utils.*;
import org.bitcoinj.wallet.listeners.KeyChainEventListener;
import org.slf4j.*;
import org.spongycastle.crypto.params.*;

import javax.annotation.*;
import java.security.*;
import java.util.*;
import java.util.concurrent.*;

import static com.google.common.base.Preconditions.*;

/**
 * <p>A KeyChainGroup is used by the {@link Wallet} and
 * manages: a {@link BasicKeyChain} object (which will normally be empty), and zero or more
 * {@link DeterministicKeyChain}s. A deterministic key chain will be created lazily/on demand
 * when a fresh or current key is requested, possibly being initialized from the private key bytes of the earliest non
 * rotating key in the basic key chain if one is available, or from a fresh random seed if not.</p>
 *
 * <p>If a key rotation time is set, it may be necessary to add a new DeterministicKeyChain with a fresh seed
 * and also preserve the old one, so funds can be swept from the rotating keys. In this case, there may be
 * more than one deterministic chain. The latest chain is called the active chain and is where new keys are served
 * from.</p>
 *
 * <p>The wallet delegates most key management tasks to this class. It is <b>not</b> thread safe and requires external
 * locking, i.e. by the wallet lock. The group then in turn delegates most operations to the key chain objects,
 * combining their responses together when necessary.</p>
 *
 * <p>Deterministic key chains have a concept of a lookahead size and threshold. Please see the discussion in the
 * class docs for {@link DeterministicKeyChain} for more information on this topic.</p>
 */
public class KeyChainGroup implements KeyBag {

    static {
        // Init proper random number generator, as some old Android installations have bugs that make it unsecure.
        if (Utils.isAndroidRuntime())
            new LinuxSecureRandom();
    }

    private static final Logger log = LoggerFactory.getLogger(KeyChainGroup.class);

    private BasicKeyChain basic;
    private NetworkParameters params;
    protected final LinkedList<DeterministicKeyChain> chains;
    // currentKeys is used for normal, non-multisig/married wallets. currentAddresses is used when we're handing out
    // P2SH addresses. They're mutually exclusive.
    private final EnumMap<KeyChain.KeyPurpose, DeterministicKey> currentKeys;
    private final EnumMap<KeyChain.KeyPurpose, Address> currentAddresses;
    @Nullable private KeyCrypter keyCrypter;
    private int lookaheadSize = -1;
    private int lookaheadThreshold = -1;

    /** Creates a keychain group with no basic chain, and a single, lazily created HD chain. */
    public KeyChainGroup(NetworkParameters params) {
        this(params, null, new ArrayList<DeterministicKeyChain>(1), null, null);
    }

    /** Creates a keychain group with no basic chain, and an HD chain initialized from the given seed. */
    public KeyChainGroup(NetworkParameters params, DeterministicSeed seed) {
        this(params, null, ImmutableList.of(new DeterministicKeyChain(seed)), null, null);
    }

    /**
     * Creates a keychain group with no basic chain, and an HD chain initialized from the given seed. Account path is
     * provided.
     */
    public KeyChainGroup(NetworkParameters params, DeterministicSeed seed, ImmutableList<ChildNumber> accountPath) {
        this(params, null, ImmutableList.of(new DeterministicKeyChain(seed, accountPath)), null, null);
    }

    /**
     * Creates a keychain group with no basic chain, and an HD chain that is watching the given watching key.
     * This HAS to be an account key as returned by {@link DeterministicKeyChain#getWatchingKey()}.
     */
    public KeyChainGroup(NetworkParameters params, DeterministicKey watchKey) {
        this(params, null, ImmutableList.of(DeterministicKeyChain.watch(watchKey)), null, null);
    }

    /**
     * Creates a keychain group with no basic chain, and an HD chain that is watching or spending the given key.
     * This HAS to be an account key as returned by {@link DeterministicKeyChain#getWatchingKey()}.
     */
    public KeyChainGroup(NetworkParameters params, DeterministicKey accountKey, boolean watch) {
        this(params, null, ImmutableList.of(watch ? DeterministicKeyChain.watch(accountKey) : DeterministicKeyChain.spend(accountKey)), null, null);
    }

    // Used for deserialization.
    private KeyChainGroup(NetworkParameters params, @Nullable BasicKeyChain basicKeyChain, List<DeterministicKeyChain> chains,
                          @Nullable EnumMap<KeyChain.KeyPurpose, DeterministicKey> currentKeys, @Nullable KeyCrypter crypter) {
        this.params = params;
        this.basic = basicKeyChain == null ? new BasicKeyChain() : basicKeyChain;
        this.chains = new LinkedList<>(checkNotNull(chains));
        this.keyCrypter = crypter;
        this.currentKeys = currentKeys == null
                ? new EnumMap<KeyChain.KeyPurpose, DeterministicKey>(KeyChain.KeyPurpose.class)
                : currentKeys;
        this.currentAddresses = new EnumMap<>(KeyChain.KeyPurpose.class);
        maybeLookaheadScripts();
    }

    // This keeps married redeem data in sync with the number of keys issued
    private void maybeLookaheadScripts() {
        for (DeterministicKeyChain chain : chains) {
            chain.maybeLookAheadScripts();
        }
    }

    /** Adds a new HD chain to the chains list, and make it the default chain (from which keys are issued). */
    public void createAndActivateNewHDChain() {
        // We can't do auto upgrade here because we don't know the rotation time, if any.
        final DeterministicKeyChain chain = new DeterministicKeyChain(new SecureRandom());
        addAndActivateHDChain(chain);
    }

    /**
     * Adds an HD chain to the chains list, and make it the default chain (from which keys are issued).
     * Useful for adding a complex pre-configured keychain, such as a married wallet.
     */
    public void addAndActivateHDChain(DeterministicKeyChain chain) {
        log.info("Creating and activating a new HD chain: {}", chain);
        for (ListenerRegistration<KeyChainEventListener> registration : basic.getListeners())
            chain.addEventListener(registration.listener, registration.executor);
        if (lookaheadSize >= 0)
            chain.setLookaheadSize(lookaheadSize);
        if (lookaheadThreshold >= 0)
            chain.setLookaheadThreshold(lookaheadThreshold);
        chains.add(chain);
    }

    /**
     * Returns a key that hasn't been seen in a transaction yet, and which is suitable for displaying in a wallet
     * user interface as "a convenient key to receive funds on" when the purpose parameter is
     * {@link KeyChain.KeyPurpose#RECEIVE_FUNDS}. The returned key is stable until
     * it's actually seen in a pending or confirmed transaction, at which point this method will start returning
     * a different key (for each purpose independently).
     * <p>This method is not supposed to be used for married keychains and will throw UnsupportedOperationException if
     * the active chain is married.
     * For married keychains use {@link #currentAddress(KeyChain.KeyPurpose)}
     * to get a proper P2SH address</p>
     */
    public DeterministicKey currentKey(KeyChain.KeyPurpose purpose) {
        DeterministicKeyChain chain = getActiveKeyChain();
        DeterministicKey current = currentKeys.get(purpose);
        if (current == null) {
            current = freshKey(purpose);
            currentKeys.put(purpose, current);
        }
        return current;
    }

    /**
     * Returns address for a {@link #currentKey(KeyChain.KeyPurpose)}
     */
    public Address currentAddress(KeyChain.KeyPurpose purpose) {
        return SegwitAddress.fromKey(params, currentKey(purpose));
    }

    /**
     * Returns a key that has not been returned by this method before (fresh). You can think of this as being
     * a newly created key, although the notion of "create" is not really valid for a
     * {@link DeterministicKeyChain}. When the parameter is
     * {@link KeyChain.KeyPurpose#RECEIVE_FUNDS} the returned key is suitable for being put
     * into a receive coins wizard type UI. You should use this when the user is definitely going to hand this key out
     * to someone who wishes to send money.
     * <p>This method is not supposed to be used for married keychains and will throw UnsupportedOperationException if
     * the active chain is married.
     * For married keychains use {@link #freshAddress(KeyChain.KeyPurpose)}
     * to get a proper P2SH address</p>
     */
    public DeterministicKey freshKey(KeyChain.KeyPurpose purpose) {
        return freshKeys(purpose, 1).get(0);
    }

    /**
     * Returns a key/s that have not been returned by this method before (fresh). You can think of this as being
     * newly created key/s, although the notion of "create" is not really valid for a
     * {@link DeterministicKeyChain}. When the parameter is
     * {@link KeyChain.KeyPurpose#RECEIVE_FUNDS} the returned key is suitable for being put
     * into a receive coins wizard type UI. You should use this when the user is definitely going to hand this key out
     * to someone who wishes to send money.
     * <p>This method is not supposed to be used for married keychains and will throw UnsupportedOperationException if
     * the active chain is married.
     * For married keychains use {@link #freshAddress(KeyChain.KeyPurpose)}
     * to get a proper P2SH address</p>
     */
    public List<DeterministicKey> freshKeys(KeyChain.KeyPurpose purpose, int numberOfKeys) {
        DeterministicKeyChain chain = getActiveKeyChain();
        return chain.getKeys(purpose, numberOfKeys);   // Always returns the next key along the key chain.
    }

    /**
     * Returns address for a {@link #freshKey(KeyChain.KeyPurpose)}
     */
    public Address freshAddress(KeyChain.KeyPurpose purpose) {
        return SegwitAddress.fromKey(params, freshKey(purpose));
    }

    /** Returns the key chain that's used for generation of fresh/current keys. This is always the newest HD chain. */
    public final DeterministicKeyChain getActiveKeyChain() {
        if (chains.isEmpty()) {
            if (basic.numKeys() > 0) {
                log.warn("No HD chain present but random keys are: you probably deserialized an old wallet.");
                // If called from the wallet (most likely) it'll try to upgrade us, as it knows the rotation time
                // but not the password.
                throw new DeterministicUpgradeRequiredException();
            }
            // Otherwise we have no HD chains and no random keys: we are a new born! So a random seed is fine.
            createAndActivateNewHDChain();
        }
        return chains.get(chains.size() - 1);
    }

    /**
     * Sets the lookahead buffer size for ALL deterministic key chains as well as for following key chains if any exist,
     * see {@link DeterministicKeyChain#setLookaheadSize(int)}
     * for more information.
     */
    public void setLookaheadSize(int lookaheadSize) {
        this.lookaheadSize = lookaheadSize;
        for (DeterministicKeyChain chain : chains) {
            chain.setLookaheadSize(lookaheadSize);
        }
    }

    /**
     * Gets the current lookahead size being used for ALL deterministic key chains. See
     * {@link DeterministicKeyChain#setLookaheadSize(int)}
     * for more information.
     */
    public int getLookaheadSize() {
        if (lookaheadSize == -1)
            return getActiveKeyChain().getLookaheadSize();
        else
            return lookaheadSize;
    }

    /**
     * Sets the lookahead buffer threshold for ALL deterministic key chains, see
     * {@link DeterministicKeyChain#setLookaheadThreshold(int)}
     * for more information.
     */
    public void setLookaheadThreshold(int num) {
        for (DeterministicKeyChain chain : chains) {
            chain.setLookaheadThreshold(num);
        }
    }

    /**
     * Gets the current lookahead threshold being used for ALL deterministic key chains. See
     * {@link DeterministicKeyChain#setLookaheadThreshold(int)}
     * for more information.
     */
    public int getLookaheadThreshold() {
        if (lookaheadThreshold == -1)
            return getActiveKeyChain().getLookaheadThreshold();
        else
            return lookaheadThreshold;
    }

    /** Imports the given keys into the basic chain, creating it if necessary. */
    public int importKeys(List<ECKey> keys) {
        return basic.importKeys(keys);
    }

    /** Imports the given keys into the basic chain, creating it if necessary. */
    public int importKeys(ECKey... keys) {
        return importKeys(ImmutableList.copyOf(keys));
    }

    @Override
    @Nullable
    public RedeemData findRedeemDataFromScriptHash(byte[] scriptHash) {
        // Iterate in reverse order, since the active keychain is the one most likely to have the hit
        for (Iterator<DeterministicKeyChain> iter = chains.descendingIterator() ; iter.hasNext() ; ) {
            DeterministicKeyChain chain = iter.next();
            RedeemData redeemData = chain.findRedeemDataByScriptHash(ByteString.copyFrom(scriptHash));
            if (redeemData != null)
                return redeemData;
        }
        return null;
    }

    public void markP2SHAddressAsUsed(LegacyAddress address) {
        checkArgument(address.getOutputScriptType() == ScriptType.P2SH);
        RedeemData data = findRedeemDataFromScriptHash(address.getHash());
        if (data == null)
            return;   // Not our P2SH address.
        for (ECKey key : data.keys) {
            for (DeterministicKeyChain chain : chains) {
                DeterministicKey k = chain.findKeyFromPubKey(key.getPubKey());
                if (k == null) continue;
                chain.markKeyAsUsed(k);
                maybeMarkCurrentAddressAsUsed(address);
            }
        }
    }

    @Nullable
    @Override
    public ECKey findKeyFromPubHash(byte[] pubkeyHash) {
        ECKey result;
        if ((result = basic.findKeyFromPubHash(pubkeyHash)) != null)
            return result;
        for (DeterministicKeyChain chain : chains) {
            if ((result = chain.findKeyFromPubHash(pubkeyHash)) != null)
                return result;
        }
        return null;
    }

    /**
     * Mark the DeterministicKeys as used, if they match the pubkeyHash
     * See {@link DeterministicKeyChain#markKeyAsUsed(DeterministicKey)} for more info on this.
     */
    public void markPubKeyHashAsUsed(byte[] pubkeyHash) {
        for (DeterministicKeyChain chain : chains) {
            DeterministicKey key;
            if ((key = chain.markPubHashAsUsed(pubkeyHash)) != null) {
                maybeMarkCurrentKeyAsUsed(key);
                return;
            }
        }
    }

    /** If the given P2SH address is "current", advance it to a new one. */
    private void maybeMarkCurrentAddressAsUsed(LegacyAddress address) {
        checkArgument(address.getOutputScriptType() == ScriptType.P2SH);
        for (Map.Entry<KeyChain.KeyPurpose, Address> entry : currentAddresses.entrySet()) {
            if (entry.getValue() != null && entry.getValue().equals(address)) {
                log.info("Marking P2SH address as used: {}", address);
                currentAddresses.put(entry.getKey(), freshAddress(entry.getKey()));
                return;
            }
        }
    }

    /** If the given key is "current", advance the current key to a new one. */
    private void maybeMarkCurrentKeyAsUsed(DeterministicKey key) {
        // It's OK for currentKeys to be empty here: it means we're a married wallet and the key may be a part of a
        // rotating chain.
        for (Map.Entry<KeyChain.KeyPurpose, DeterministicKey> entry : currentKeys.entrySet()) {
            if (entry.getValue() != null && entry.getValue().equals(key)) {
                log.info("Marking key as used: {}", key);
                currentKeys.put(entry.getKey(), freshKey(entry.getKey()));
                return;
            }
        }
    }

    public boolean hasKey(ECKey key) {
        if (basic.hasKey(key))
            return true;
        for (DeterministicKeyChain chain : chains)
            if (chain.hasKey(key))
                return true;
        return false;
    }

    @Nullable
    @Override
    public ECKey findKeyFromPubKey(byte[] pubkey) {
        ECKey result;
        if ((result = basic.findKeyFromPubKey(pubkey)) != null)
            return result;
        for (DeterministicKeyChain chain : chains) {
            if ((result = chain.findKeyFromPubKey(pubkey)) != null)
                return result;
        }
        return null;
    }

    /**
     * Mark the DeterministicKeys as used, if they match the pubkey
     * See {@link DeterministicKeyChain#markKeyAsUsed(DeterministicKey)} for more info on this.
     */
    public void markPubKeyAsUsed(byte[] pubkey) {
        for (DeterministicKeyChain chain : chains) {
            DeterministicKey key;
            if ((key = chain.markPubKeyAsUsed(pubkey)) != null) {
                maybeMarkCurrentKeyAsUsed(key);
                return;
            }
        }
    }

    /** Returns the number of keys managed by this group, including the lookahead buffers. */
    public int numKeys() {
        int result = basic.numKeys();
        for (DeterministicKeyChain chain : chains)
            result += chain.numKeys();
        return result;
    }

    /**
     * Removes a key that was imported into the basic key chain. You cannot remove deterministic keys.
     * @throws java.lang.IllegalArgumentException if the key is deterministic.
     */
    public boolean removeImportedKey(ECKey key) {
        checkNotNull(key);
        checkArgument(!(key instanceof DeterministicKey));
        return basic.removeKey(key);
    }

    /** Returns true if the group is encrypted. */
    public boolean isEncrypted() {
        return keyCrypter != null;
    }

    /**
     * Returns whether this chain has only watching keys (unencrypted keys with no private part). Mixed chains are
     * forbidden.
     * 
     * @throws IllegalStateException if there are no keys, or if there is a mix between watching and non-watching keys.
     */
    public boolean isWatching() {
        BasicKeyChain.State basicState = basic.isWatching();
        BasicKeyChain.State activeState = BasicKeyChain.State.EMPTY;
        if (!chains.isEmpty()) {
            if (getActiveKeyChain().isWatching())
                activeState = BasicKeyChain.State.WATCHING;
            else
                activeState = BasicKeyChain.State.REGULAR;
        }
        if (basicState == BasicKeyChain.State.EMPTY) {
            if (activeState == BasicKeyChain.State.EMPTY)
                throw new IllegalStateException("Empty key chain group: cannot answer isWatching() query");
            return activeState == BasicKeyChain.State.WATCHING;
        } else if (activeState == BasicKeyChain.State.EMPTY)
            return basicState == BasicKeyChain.State.WATCHING;
        else {
            if (activeState != basicState)
                throw new IllegalStateException("Mix of watching and non-watching keys in wallet");
            return activeState == BasicKeyChain.State.WATCHING;
        }
    }

    /** Returns the key crypter or null if the group is not encrypted. */
    @Nullable public KeyCrypter getKeyCrypter() { return keyCrypter; }

    /**
     * Returns a list of the non-deterministic keys that have been imported into the wallet, or the empty list if none.
     */
    public List<ECKey> getImportedKeys() {
        return basic.getKeys();
    }

    public long getEarliestKeyCreationTime() {
        long time = basic.getEarliestKeyCreationTime();   // Long.MAX_VALUE if empty.
        for (DeterministicKeyChain chain : chains)
            time = Math.min(time, chain.getEarliestKeyCreationTime());
        return time;
    }

    public int getBloomFilterElementCount() {
        int result = basic.numBloomFilterEntries();
        for (DeterministicKeyChain chain : chains) {
            result += chain.numBloomFilterEntries();
        }
        return result;
    }

    public BloomFilter getBloomFilter(int size, double falsePositiveRate, long nTweak) {
        BloomFilter filter = new BloomFilter(size, falsePositiveRate, nTweak);
        if (basic.numKeys() > 0)
            filter.merge(basic.getFilter(size, falsePositiveRate, nTweak));

        for (DeterministicKeyChain chain : chains) {
            filter.merge(chain.getFilter(size, falsePositiveRate, nTweak));
        }
        return filter;
    }

    public boolean isRequiringUpdateAllBloomFilter() {
        throw new UnsupportedOperationException();   // Unused.
    }

    private Script makeP2SHOutputScript(DeterministicKey followedKey, DeterministicKeyChain chain) {
        return ScriptBuilder.createP2SHOutputScript(chain.getRedeemData(followedKey).redeemScript);
    }

    /** Adds a listener for events that are run when keys are added, on the user thread. */
    public void addEventListener(KeyChainEventListener listener) {
        addEventListener(listener, Threading.USER_THREAD);
    }

    /** Adds a listener for events that are run when keys are added, on the given executor. */
    public void addEventListener(KeyChainEventListener listener, Executor executor) {
        checkNotNull(listener);
        checkNotNull(executor);
        basic.addEventListener(listener, executor);
        for (DeterministicKeyChain chain : chains)
            chain.addEventListener(listener, executor);
    }

    /** Removes a listener for events that are run when keys are added. */
    public boolean removeEventListener(KeyChainEventListener listener) {
        checkNotNull(listener);
        for (DeterministicKeyChain chain : chains)
            chain.removeEventListener(listener);
        return basic.removeEventListener(listener);
    }

    /** Returns a list of key protobufs obtained by merging the chains. */
    public List<Protos.Key> serializeToProtobuf() {
        List<Protos.Key> result;
        if (basic != null)
            result = basic.serializeToProtobuf();
        else
            result = Lists.newArrayList();
        for (DeterministicKeyChain chain : chains) {
            List<Protos.Key> protos = chain.serializeToProtobuf();
            result.addAll(protos);
        }
        return result;
    }

    static KeyChainGroup fromProtobufUnencrypted(NetworkParameters params, List<Protos.Key> keys) throws UnreadableWalletException {
        return fromProtobufUnencrypted(params, keys, new DefaultKeyChainFactory());
    }

    public static KeyChainGroup fromProtobufUnencrypted(NetworkParameters params, List<Protos.Key> keys, KeyChainFactory factory) throws UnreadableWalletException {
        BasicKeyChain basicKeyChain = BasicKeyChain.fromProtobufUnencrypted(keys);
        List<DeterministicKeyChain> chains = DeterministicKeyChain.fromProtobuf(keys, null, factory);
        EnumMap<KeyChain.KeyPurpose, DeterministicKey> currentKeys = null;
        if (!chains.isEmpty())
            currentKeys = createCurrentKeysMap(chains);
        extractFollowingKeychains(chains);
        return new KeyChainGroup(params, basicKeyChain, chains, currentKeys, null);
    }

    static KeyChainGroup fromProtobufEncrypted(NetworkParameters params, List<Protos.Key> keys, KeyCrypter crypter) throws UnreadableWalletException {
        return fromProtobufEncrypted(params, keys, crypter, new DefaultKeyChainFactory());
    }

    public static KeyChainGroup fromProtobufEncrypted(NetworkParameters params, List<Protos.Key> keys, KeyCrypter crypter, KeyChainFactory factory) throws UnreadableWalletException {
        checkNotNull(crypter);
        BasicKeyChain basicKeyChain = BasicKeyChain.fromProtobufEncrypted(keys, crypter);
        List<DeterministicKeyChain> chains = DeterministicKeyChain.fromProtobuf(keys, crypter, factory);
        EnumMap<KeyChain.KeyPurpose, DeterministicKey> currentKeys = null;
        if (!chains.isEmpty())
            currentKeys = createCurrentKeysMap(chains);
        extractFollowingKeychains(chains);
        return new KeyChainGroup(params, basicKeyChain, chains, currentKeys, crypter);
    }

    private static EnumMap<KeyChain.KeyPurpose, DeterministicKey> createCurrentKeysMap(List<DeterministicKeyChain> chains) {
        DeterministicKeyChain activeChain = chains.get(chains.size() - 1);

        EnumMap<KeyChain.KeyPurpose, DeterministicKey> currentKeys = new EnumMap<>(KeyChain.KeyPurpose.class);

        // assuming that only RECEIVE and CHANGE keys are being used at the moment, we will treat latest issued external key
        // as current RECEIVE key and latest issued internal key as CHANGE key. This should be changed as soon as other
        // kinds of KeyPurpose are introduced.
        if (activeChain.getIssuedExternalKeys() > 0) {
            DeterministicKey currentExternalKey = activeChain.getKeyByPath(
                    HDUtils.append(
                            HDUtils.concat(activeChain.getAccountPath(), DeterministicKeyChain.EXTERNAL_SUBPATH),
                            new ChildNumber(activeChain.getIssuedExternalKeys() - 1)));
            currentKeys.put(KeyChain.KeyPurpose.RECEIVE_FUNDS, currentExternalKey);
        }

        if (activeChain.getIssuedInternalKeys() > 0) {
            DeterministicKey currentInternalKey = activeChain.getKeyByPath(
                    HDUtils.append(
                            HDUtils.concat(activeChain.getAccountPath(), DeterministicKeyChain.INTERNAL_SUBPATH),
                            new ChildNumber(activeChain.getIssuedInternalKeys() - 1)));
            currentKeys.put(KeyChain.KeyPurpose.CHANGE, currentInternalKey);
        }
        return currentKeys;
    }

    private static void extractFollowingKeychains(List<DeterministicKeyChain> chains) {
        // look for following key chains and map them to the watch keys of followed keychains
        for (Iterator<DeterministicKeyChain> it = chains.iterator(); it.hasNext(); ) {
            DeterministicKeyChain chain = it.next();
            if (chain.isFollowing()) {
                it.remove();
            }
        }
    }

    public String toString(boolean includePrivateKeys, @Nullable KeyParameter aesKey) {
        final StringBuilder builder = new StringBuilder();
        if (basic != null) {
            List<ECKey> keys = basic.getKeys();
            Collections.sort(keys, ECKey.AGE_COMPARATOR);
            for (ECKey key : keys)
                key.formatKeyWithAddress(includePrivateKeys, aesKey, builder, params);
        }
        for (DeterministicKeyChain chain : chains)
            builder.append(chain.toString(includePrivateKeys, aesKey, params)).append('\n');
        return builder.toString();
    }

    /** Returns a copy of the current list of chains. */
    public List<DeterministicKeyChain> getDeterministicKeyChains() {
        return new ArrayList<>(chains);
    }
    /**
     * Returns a counter that increases (by an arbitrary amount) each time new keys have been calculated due to
     * lookahead and thus the Bloom filter that was previously calculated has become stale.
     */
    public int getCombinedKeyLookaheadEpochs() {
        int epoch = 0;
        for (DeterministicKeyChain chain : chains)
            epoch += chain.getKeyLookaheadEpoch();
        return epoch;
    }
}
