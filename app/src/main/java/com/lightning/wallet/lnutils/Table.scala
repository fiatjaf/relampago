package com.lightning.wallet.lnutils

import spray.json._
import com.lightning.wallet.lnutils.ImplicitJsonFormats._
import com.lightning.wallet.ln.Tools.{none, runAnd}

import com.lightning.wallet.lnutils.olympus.CloudData
import net.sqlcipher.database.SQLiteDatabase
import android.content.Context
import android.net.Uri


object OlympusTable extends Table {
  val (table, identifier, url, data, auth, order, removable) = ("olympus", "identifier", "url", "data", "auth", "ord", "removable")
  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $url, $data, $auth, $order, $removable) VALUES (?, ?, ?, ?, ?, ?)"
  val updMetaSql = s"UPDATE $table SET $url = ?, $auth = ? WHERE $identifier = ?"
  val updOrderSql = s"UPDATE $table SET $order = ? WHERE $identifier = ?"
  val updDataSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $order ASC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $identifier TEXT NOT NULL UNIQUE,
      $url TEXT NOT NULL UNIQUE,
      $data TEXT NOT NULL,
      $auth INTEGER NOT NULL,
      $order INTEGER NOT NULL,
      $removable INTEGER NOT NULL
    )"""
}

object ChannelTable extends Table {
  val (table, identifier, data) = ("channel", "identifier", "data")
  val updSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $id DESC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val newSql = s"""
    INSERT OR IGNORE INTO $table
    ($identifier, $data) VALUES (?, ?)"""

  val createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $identifier TEXT NOT NULL UNIQUE,
      $data TEXT NOT NULL
    )"""
}

object PaymentTable extends Table {
  import com.lightning.wallet.ln.PaymentInfo.{HIDDEN, SUCCESS, FAILURE, WAITING}
  val Tuple12(table, hash, preimage, incoming, msat, status, stamp, description, pr, rd, search, limit) =
    ("payment", "hash", "preimage", "incoming", "msat", "status", "stamp", "description", "pr", "rd", "search", 24)

  // Inserting
  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"
  val insert9 = s"$hash, $preimage, $incoming, $msat, $status, $stamp, $description, $pr, $rd"
  val newSql = s"INSERT OR IGNORE INTO $table ($insert9) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"

  // Selecting
  val selectSql = s"SELECT * FROM $table WHERE $hash = ?"
  val selectRecentSql = s"SELECT * FROM $table WHERE $status <> $HIDDEN ORDER BY $id DESC LIMIT $limit"
  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT DISTINCT $hash FROM $fts$table WHERE $search MATCH ? LIMIT $limit)"

  // Updating
  val updRoutingSql = s"UPDATE $table SET $rd = ? WHERE $hash = ?"
  val updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"
  val updFailAllWaitingSql = s"UPDATE $table SET $status = $FAILURE WHERE $status = $WAITING"
  val updOkOutgoingSql = s"UPDATE $table SET $status = $SUCCESS, $preimage = ? WHERE $hash = ?"
  val updOkIncomingSql = s"UPDATE $table SET $status = $SUCCESS, $msat = ?, $stamp = ? WHERE $hash = ?"

  // Creating
  val createVSql = s"""
    CREATE VIRTUAL TABLE $fts$table
    USING $fts($search, $hash)"""

  val createSql = s"""
    CREATE TABLE $table(
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $hash STRING UNIQUE NOT NULL,
      $preimage STRING NOT NULL,
      $incoming INTEGER NOT NULL,
      $msat INTEGER NOT NULL,
      $status INTEGER NOT NULL,
      $stamp INTEGER NOT NULL,
      $description STRING NOT NULL,
      $pr STRING NOT NULL,
      $rd STRING NOT NULL
    );
    CREATE INDEX idx1 ON $table ($status);
    CREATE INDEX idx2 ON $table ($hash);
    COMMIT"""
}

trait Table { val (id, fts) = "_id" -> "fts4" }
class CipherOpenHelper(context: Context, name: String, secret: String)
extends net.sqlcipher.database.SQLiteOpenHelper(context, name, null, 1) {

  SQLiteDatabase loadLibs context
  val base = getWritableDatabase(secret)
  def onUpgrade(db: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: Any*) = base.execSQL(sql, params.map(_.toString).toArray)
  def select(sql: String, params: Any*) = base.rawQuery(sql, params.map(_.toString).toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.wallet/table/$tbl"

  def txWrap(process: => Unit) = try {
    runAnd(base.beginTransaction)(process)
    base.setTransactionSuccessful
  } finally base.endTransaction

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL PaymentTable.createVSql
    dbs execSQL PaymentTable.createSql
    dbs execSQL ChannelTable.createSql
    dbs execSQL OlympusTable.createSql

    val emptyData = CloudData(None, Vector.empty, Vector.empty).toJson.toString
    val main: Array[AnyRef] = Array("main-dev-server", "http://213.133.99.89:9002", emptyData, "1", "0", "0")
    val fallback: Array[AnyRef] = Array("fallback-dev-server", "http://10.0.2.2:9002", emptyData, "0", "1", "0")

    dbs.execSQL(OlympusTable.newSql, main)
    dbs.execSQL(OlympusTable.newSql, fallback)
  }
}