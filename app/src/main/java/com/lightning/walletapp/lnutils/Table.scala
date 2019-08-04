package com.lightning.walletapp.lnutils

import spray.json._
import android.database.sqlite._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.Tools.{random, runAnd}
import com.lightning.walletapp.lnutils.olympus.CloudData
import android.content.Context
import scodec.bits.ByteVector
import android.net.Uri


object OlympusTable extends Table {
  val (table, identifier, url, data, auth, order, removable) = ("olympus", "identifier", "url", "data", "auth", "ord", "removable")
  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $url, $data, $auth, $order, $removable) VALUES (?, ?, ?, ?, ?, ?)"
  val updMetaSql = s"UPDATE $table SET $url = ?, $auth = ?, $order = ? WHERE $identifier = ?"
  val updDataSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $order ASC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $identifier TEXT NOT NULL UNIQUE,
      $url STRING NOT NULL UNIQUE, $data STRING NOT NULL, $auth INTEGER NOT NULL,
      $order INTEGER NOT NULL, $removable INTEGER NOT NULL
    )"""
}

object OlympusLogTable extends Table {
  val (table, tokensUsed, explanation, stamp) = ("olympuslog", "tokensused", "explanation", "stamp")
  val newSql = s"INSERT INTO $table ($tokensUsed, $explanation, $stamp) VALUES (?, ?, ?)"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $stamp DESC LIMIT 6"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $tokensUsed INTEGER NOT NULL,
      $explanation STRING NOT NULL, $stamp INTEGER NOT NULL
    )"""
}

object ChannelTable extends Table {
  val Tuple3(table, identifier, data) = Tuple3("channel", "identifier", "data")
  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $data) VALUES (?, ?)"
  val updSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $id DESC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $identifier TEXT NOT NULL UNIQUE,
      $data STRING NOT NULL
    )"""
}

object BadEntityTable extends Table {
  val (table, resId, expire, amount) = ("badentity", "resId", "expire", "amount")
  val newSql = s"INSERT OR IGNORE INTO $table ($resId, $expire, $amount) VALUES (?, ?, ?)"
  val selectSql = s"SELECT * FROM $table WHERE $expire > ? AND $amount <= ? LIMIT 320"
  val updSql = s"UPDATE $table SET $expire = ?, $amount = ? WHERE $resId = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $resId STRING NOT NULL UNIQUE,
      $expire INTEGER NOT NULL,
      $amount INTEGER NOT NULL
    );

    /* resId index is created automatically because this field is UNIQUE */
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($expire, $amount);
    COMMIT
    """
}

object RouteTable extends Table {
  val (table, path, targetNode) = Tuple3("route", "path", "targetNode")
  val newSql = s"INSERT OR IGNORE INTO $table ($path, $targetNode) VALUES (?, ?)"
  val updSql = s"UPDATE $table SET $path = ? WHERE $targetNode = ?"
  val selectSql = s"SELECT * FROM $table WHERE $targetNode = ?"
  val killSql = s"DELETE FROM $table WHERE $targetNode = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $targetNode STRING NOT NULL UNIQUE,
      $path STRING NOT NULL
    )"""
}

object PaymentTable extends Table {
  val (search, table, pr, preimage, incoming, status, stamp) = ("search", "payment", "pr", "preimage", "incoming", "status", "stamp")
  val (description, hash, firstMsat, lastMsat, lastExpiry, chanId) = ("description", "hash", "firstMsat", "lastMsat", "lastExpiry", "chanId")
  val insert11 = s"$pr, $preimage, $incoming, $status, $stamp, $description, $hash, $firstMsat, $lastMsat, $lastExpiry, $chanId"
  val newSql = s"INSERT OR IGNORE INTO $table ($insert11) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"

  // Selecting
  val selectSql = s"SELECT * FROM $table WHERE $hash = ?"
  val selectRecentSql = s"SELECT * FROM $table ORDER BY $id DESC LIMIT 48"
  val selectPaymentNumSql = s"SELECT count($hash) FROM $table WHERE $status = $SUCCESS AND $chanId = ?"
  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $search MATCH ? LIMIT 96)"

  // Updating, creating, removing
  val updOkOutgoingSql = s"UPDATE $table SET $status = $SUCCESS, $preimage = ?, $chanId = ? WHERE $hash = ?"
  val updOkIncomingSql = s"UPDATE $table SET $status = $SUCCESS, $firstMsat = ?, $stamp = ?, $chanId = ? WHERE $hash = ?"
  val updLastParamsSql = s"UPDATE $table SET $status = $WAITING, $firstMsat = ?, $lastMsat = ?, $lastExpiry = ? WHERE $hash = ?"
  // Broken in-flight payment may become fulfilled but then get overridden on restart unless we check for SUCCESS
  val updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ? AND $status <> $SUCCESS"
  val killSql = s"DELETE FROM $table WHERE $hash = ? AND $status <> $SUCCESS"

  val updFailWaitingSql = s"""
    UPDATE $table SET $status = $FAILURE /* fail those payments which... */
    WHERE ($status = $WAITING AND $incoming = 0) /* outgoing and pending or broken */
    OR ($status = $WAITING AND $incoming = 1 AND $stamp < ?) /* incoming and expired by now */"""

  // Once incoming or outgoing payment is settled we can search it by various metadata
  val createVSql = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $hash)"

  val reCreateSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $pr STRING NOT NULL, $preimage STRING NOT NULL, $incoming INTEGER NOT NULL,
      $status INTEGER NOT NULL, $stamp INTEGER NOT NULL, $description STRING NOT NULL, $hash STRING NOT NULL UNIQUE,
      $firstMsat INTEGER NOT NULL, $lastMsat INTEGER NOT NULL, $lastExpiry INTEGER NOT NULL, $chanId STRING NOT NULL
    );

    DROP INDEX IF EXISTS idx1$table;
    /* hash index is created automatically because this field is UNIQUE */
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($status, $incoming, $stamp);
    CREATE INDEX IF NOT EXISTS idx2$table ON $table ($chanId);
    COMMIT
    """
}

object RevokedInfoTable extends Table {
  val (table, txId, chanId, myBalance, info, uploaded) = ("revokedinfo", "txid", "chanid", "mybalance", "info", "uploaded")
  val selectLocalSql = s"SELECT * FROM $table WHERE $chanId = ? AND $myBalance < ? AND $uploaded = 0 ORDER BY $myBalance ASC LIMIT 200"
  val newSql = s"INSERT INTO $table ($txId, $chanId, $myBalance, $info, $uploaded) VALUES (?, ?, ?, ?, 0)"
  val setUploadedSql = s"UPDATE $table SET $uploaded = 1 WHERE $txId = ?"
  val selectTxIdSql = s"SELECT * FROM $table WHERE $txId = ?"
  val killSql = s"DELETE FROM $table WHERE $chanId = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $txId STRING NOT NULL,
      $chanId STRING NOT NULL, $myBalance INTEGER NOT NULL,
      $info STRING NOT NULL, $uploaded INTEGER NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx2$table ON $table ($chanId, $myBalance, $uploaded);
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($txId);
    COMMIT
    """
}

trait Table { val (id, fts) = "_id" -> "fts4" }
class LNOpenHelper(context: Context, name: String)
extends SQLiteOpenHelper(context, name, null, 5) {

  val base = getWritableDatabase
  val asString: Any => String = {
    case byteVec: ByteVector => byteVec.toHex
    case otherwise => otherwise.toString
  }

  def change(sql: String, params: Any*) = base.execSQL(sql, params.map(asString).toArray)
  def select(sql: String, params: Any*) = base.rawQuery(sql, params.map(asString).toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.walletapp/table/$tbl"

  def txWrap(run: => Unit) = try {
    runAnd(base.beginTransaction)(run)
    base.setTransactionSuccessful
  } finally base.endTransaction

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL RevokedInfoTable.createSql
    dbs execSQL BadEntityTable.createSql
    dbs execSQL PaymentTable.reCreateSql
    dbs execSQL PaymentTable.createVSql
    dbs execSQL ChannelTable.createSql
    dbs execSQL RouteTable.createSql

    dbs execSQL OlympusLogTable.createSql
    dbs execSQL OlympusTable.createSql

    // Randomize an order of two available default servers
    val (ord1, ord2) = if (random.nextBoolean) ("0", "1") else ("1", "0")
    val emptyData = CloudData(info = None, tokens = Vector.empty, acts = Vector.empty).toJson.toString
    val dev1: Array[AnyRef] = Array("server-1", "https://a.lightning-wallet.com:9103", emptyData, "1", ord1, "0")
    val dev2: Array[AnyRef] = Array("server-2", "https://b.lightning-wallet.com:9103", emptyData, "0", ord2, "1")
    dbs.execSQL(OlympusTable.newSql, dev1)
    dbs.execSQL(OlympusTable.newSql, dev2)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int) = {
    // Old version of RouteTable had a useless expiry column
    dbs execSQL s"DROP TABLE IF EXISTS ${RouteTable.table}"

    // Should work even for updates across many version ranges
    // because each table and index has CREATE IF EXISTS prefix
    dbs execSQL RevokedInfoTable.createSql
    dbs execSQL OlympusLogTable.createSql
    dbs execSQL PaymentTable.reCreateSql
    dbs execSQL RouteTable.createSql
  }
}