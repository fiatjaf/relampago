package com.lightning.walletapp.lnutils

import spray.json._
import android.database.sqlite._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.Tools.{random, runAnd}
import com.lightning.walletapp.lnutils.olympus.CloudData
import android.content.Context
import android.net.Uri


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

    /* resId index is created automatically because it's UNIQUE */
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($expire, $amount);
    COMMIT
    """
}

object RouteTable extends Table {
  val (table, path, targetNode, expire) = Tuple4("route", "path", "targetNode", "expire")
  val newSql = s"INSERT OR IGNORE INTO $table ($path, $targetNode, $expire) VALUES (?, ?, ?)"
  val updSql = s"UPDATE $table SET $path = ?, $expire = ? WHERE $targetNode = ?"
  val selectSql = s"SELECT * FROM $table WHERE $targetNode = ? AND $expire > ?"
  val killSql = s"DELETE FROM $table WHERE $targetNode = ?"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $path STRING NOT NULL,
      $targetNode STRING NOT NULL UNIQUE, $expire INTEGER NOT NULL
    );

    /* targetNode index is created automatically because it's UNIQUE */
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($targetNode, $expire);
    COMMIT
    """
}

object PaymentTable extends Table {
  val (search, table, pr, preimage, incoming, status, stamp) = ("search", "payment", "pr", "preimage", "incoming", "status", "stamp")
  val (description, hash, firstMsat, lastMsat, lastExpiry, chanId) = ("description", "hash", "firstMsat", "lastMsat", "lastExpiry", "chanId")
  val insert11 = s"$pr, $preimage, $incoming, $status, $stamp, $description, $hash, $firstMsat, $lastMsat, $lastExpiry, $chanId"
  val newSql = s"INSERT OR IGNORE INTO $table ($insert11) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"

  // Selecting
  val selectSql = s"SELECT * FROM $table WHERE $hash = ?"
  val selectStatSql = s"SELECT SUM($lastMsat), SUM($firstMsat) FROM $table WHERE $chanId = ? AND $incoming = ?"
  val selectRecentSql = s"SELECT * FROM $table WHERE $status IN ($WAITING, $SUCCESS, $FAILURE, $FROZEN) ORDER BY $id DESC LIMIT 48"
  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $search MATCH ? LIMIT 24)"

  // Updating, creating
  val updOkOutgoingSql = s"UPDATE $table SET $status = $SUCCESS, $preimage = ?, $chanId = ? WHERE $hash = ?"
  val updOkIncomingSql = s"UPDATE $table SET $status = $SUCCESS, $firstMsat = ?, $stamp = ?, $chanId = ? WHERE $hash = ?"
  val updLastParamsSql = s"UPDATE $table SET $status = $WAITING, $firstMsat = ?, $lastMsat = ?, $lastExpiry = ? WHERE $hash = ?"
  val updFailWaitingAndFrozenSql = s"UPDATE $table SET $status = $FAILURE WHERE $status IN ($WAITING, $FROZEN)"
  val updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"

  // Once incoming or outgoing payment is settled we can search it by various metadata
  val createVSql = s"CREATE VIRTUAL TABLE IF NOT EXISTS $fts$table USING $fts($search, $hash)"

  val createSql = s"""
    CREATE TABLE IF NOT EXISTS $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $pr STRING NOT NULL, $preimage STRING NOT NULL, $incoming INTEGER NOT NULL,
      $status INTEGER NOT NULL, $stamp INTEGER NOT NULL, $description STRING NOT NULL, $hash STRING NOT NULL UNIQUE,
      $firstMsat INTEGER NOT NULL, $lastMsat INTEGER NOT NULL, $lastExpiry INTEGER NOT NULL, $chanId STRING NOT NULL
    );

    /* hash index is created automatically because it's UNIQUE */
    CREATE INDEX IF NOT EXISTS idx1$table ON $table ($status);
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
abstract class TableHelper(ctxt: Context, name: String, v: Int) extends SQLiteOpenHelper(ctxt, name, null, v) {
  // BinaryData and PublicKey should always yield raw strings for change method to work because of SQLite conversions
  val base = getWritableDatabase

  def change(sql: String, params: Any*) = base.execSQL(sql, params.map(_.toString).toArray)
  def select(sql: String, params: Any*) = base.rawQuery(sql, params.map(_.toString).toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.walletapp/table/$tbl"

  def txWrap(run: => Unit) = try {
    runAnd(base.beginTransaction)(run)
    base.setTransactionSuccessful
  } finally base.endTransaction
}

class LNExtHelper(context: Context, name: String) extends TableHelper(context, name, 1) {
  // Tables in this database contain disposable data which may be dropped in case of phone loss

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL OlympusLogTable.createSql
    dbs execSQL RevokedInfoTable.createSql
    dbs execSQL BadEntityTable.createSql
    dbs execSQL PaymentTable.createVSql
    dbs execSQL PaymentTable.createSql
    dbs execSQL RouteTable.createSql
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int) = {
    // Should work even for updates across many version ranges
    // because each table and index has CREATE IF EXISTS prefix
  }
}

class LNCoreHelper(context: Context, name: String) extends TableHelper(context, name, 4) {
  // Tables in this database contain critical information which has to be small and transferrable

  def onCreate(dbs: SQLiteDatabase) = {
    // First create channel and olympus tables
    // then prefill olympus with default servers
    dbs execSQL ChannelTable.createSql
    dbs execSQL OlympusTable.createSql

    val (ord1, ord2) = if (random.nextBoolean) ("0", "1") else ("1", "0")
    val emptyData = CloudData(info = None, tokens = Vector.empty, acts = Vector.empty).toJson.toString
    val dev1: Array[AnyRef] = Array("server-1", "https://a.lightning-wallet.com:9103", emptyData, "1", ord1, "0")
    val dev2: Array[AnyRef] = Array("server-2", "https://b.lightning-wallet.com:9103", emptyData, "0", ord2, "1")
    dbs.execSQL(OlympusTable.newSql, dev1)
    dbs.execSQL(OlympusTable.newSql, dev2)
  }

  def onUpgrade(dbs: SQLiteDatabase, v0: Int, v1: Int) = {
    // Since we need to support even the oldest installations
    // we walk through every historical database upgrade

    if (v0 < 3) {
      // Create this table if it's not in core db
      dbs execSQL RevokedInfoTable.createSql
    }

    if (v0 < 4) {
      // We have tables in core db which have to be copied into identical tables of extended db
      val extFilePath = context.getDatabasePath(com.lightning.walletapp.Utils.dbExtFile).getPath
      dbs.setTransactionSuccessful
      dbs.endTransaction

      // Attach fresh extended db while not in transaction
      dbs.execSQL(s"ATTACH DATABASE '$extFilePath' AS ext")
      dbs.beginTransaction

      // While in a middle of transaction we copy filled tables from this db info fresh extended db
      dbs.execSQL(s"INSERT INTO ext.${RevokedInfoTable.table} SELECT * FROM ${RevokedInfoTable.table}")
      dbs.execSQL(s"INSERT INTO ext.${PaymentTable.table} SELECT * FROM ${PaymentTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${PaymentTable.fts}${PaymentTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${RevokedInfoTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${OlympusLogTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${BadEntityTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${PaymentTable.table}")
      dbs.execSQL(s"DROP TABLE IF EXISTS ${RouteTable.table}")
      dbs.setTransactionSuccessful
      dbs.endTransaction

      // Detach while not in transaction
      dbs.execSQL("DETACH ext")
      dbs.beginTransaction
    }
  }
}