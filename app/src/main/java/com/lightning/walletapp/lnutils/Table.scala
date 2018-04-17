package com.lightning.walletapp.lnutils

import spray.json._
import android.database.sqlite._
import com.lightning.walletapp.ln.PaymentInfo._
import com.lightning.walletapp.lnutils.ImplicitJsonFormats._
import com.lightning.walletapp.ln.Tools.{none, random, runAnd}
import com.lightning.walletapp.lnutils.olympus.CloudData
import android.content.Context
import android.net.Uri


object OlympusTable extends Table {
  val (table, identifier, url, data, auth, order, removable) = ("olympus", "identifier", "url", "data", "auth", "ord", "removable")
  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $url, $data, $auth, $order, $removable) VALUES (?, ?, ?, ?, ?, ?)"
  val updMetaSql = s"UPDATE $table SET $url = ?, $auth = ?, $order = ? WHERE $identifier = ?"
  val updDataSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $order ASC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $identifier TEXT NOT NULL UNIQUE,
      $url STRING NOT NULL UNIQUE, $data STRING NOT NULL, $auth INTEGER NOT NULL,
      $order INTEGER NOT NULL, $removable INTEGER NOT NULL
    )"""
}

object ChannelTable extends Table {
  val Tuple3(table, identifier, data) = Tuple3("channel", "identifier", "data")
  val newSql = s"INSERT OR IGNORE INTO $table ($identifier, $data) VALUES (?, ?)"
  val updSql = s"UPDATE $table SET $data = ? WHERE $identifier = ?"
  val selectAllSql = s"SELECT * FROM $table ORDER BY $id DESC"
  val killSql = s"DELETE FROM $table WHERE $identifier = ?"

  val createSql = s"""
    CREATE TABLE $table (
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
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT,
      $resId STRING NOT NULL UNIQUE,
      $expire INTEGER NOT NULL,
      $amount INTEGER NOT NULL
    );

    /* resId index is created automatically */
    CREATE INDEX idx1$table ON $table ($expire, $amount);
    COMMIT"""
}

object RouteTable extends Table {
  val (table, path, targetNode, expire) = Tuple4("route", "path", "targetNode", "expire")
  val newSql = s"INSERT OR IGNORE INTO $table ($path, $targetNode, $expire) VALUES (?, ?, ?)"
  val updSql = s"UPDATE $table SET $path = ?, $expire = ? WHERE $targetNode = ?"
  val selectSql = s"SELECT * FROM $table WHERE $targetNode = ? AND $expire > ?"

  val createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $path STRING NOT NULL,
      $targetNode STRING NOT NULL UNIQUE, $expire INTEGER NOT NULL
    );

    /* targetNode index is created automatically */
    CREATE INDEX idx1$table ON $table ($targetNode, $expire);
    COMMIT"""
}

object PaymentTable extends Table {
  val (search, limit) = ("search", 24)
  val (table, pr, preimage, incoming, status, stamp) = ("payment", "pr", "preimage", "incoming", "status", "stamp")
  val (description, hash, firstMsat, lastMsat, lastExpiry) = ("description", "hash", "firstMsat", "lastMsat", "lastExpiry")
  val insert10 = s"$pr, $preimage, $incoming, $status, $stamp, $description, $hash, $firstMsat, $lastMsat, $lastExpiry"
  val newSql = s"INSERT OR IGNORE INTO $table ($insert10) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
  val newVirtualSql = s"INSERT INTO $fts$table ($search, $hash) VALUES (?, ?)"

  // Selecting: no need for `hash` and `id` indexes because system autogenerates those
  val selectRecentSql = s"SELECT * FROM $table WHERE $status IN ($WAITING, $SUCCESS, $FAILURE, $FROZEN) ORDER BY $id DESC LIMIT $limit"
  val searchSql = s"SELECT * FROM $table WHERE $hash IN (SELECT $hash FROM $fts$table WHERE $search MATCH ? LIMIT $limit)"
  val selectSql = s"SELECT * FROM $table WHERE $hash = ?"

  // Updating, creating
  val updLastParamsSql = s"UPDATE $table SET $status = $WAITING, $lastMsat = ?, $lastExpiry = ? WHERE $hash = ?"
  val updOkIncomingSql = s"UPDATE $table SET $status = $SUCCESS, $firstMsat = ?, $stamp = ? WHERE $hash = ?"
  val updOkOutgoingSql = s"UPDATE $table SET $status = $SUCCESS, $preimage = ? WHERE $hash = ?"
  val updFailWaitingSql = s"UPDATE $table SET $status = $FAILURE WHERE $status = $WAITING"
  val updStatusSql = s"UPDATE $table SET $status = ? WHERE $hash = ?"

  val createVSql = s"""
    CREATE VIRTUAL TABLE $fts$table
    USING $fts($search, $hash)"""

  val createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $pr STRING NOT NULL,
      $preimage STRING NOT NULL,$incoming INTEGER NOT NULL, $status INTEGER NOT NULL,
      $stamp INTEGER NOT NULL, $description STRING NOT NULL, $hash STRING NOT NULL UNIQUE,
      $firstMsat INTEGER NOT NULL, $lastMsat INTEGER NOT NULL, $lastExpiry INTEGER NOT NULL
    );

    /* hash index is created automatically */
    CREATE INDEX idx1$table ON $table ($status);
    COMMIT"""
}

object RevokedTable extends Table {
  val (table, h160, expiry, number, backup) = ("revoked", "h160", "expiry", "number", "backup")
  val newSql = s"INSERT INTO $table ($h160, $expiry, $number, $backup) VALUES (?, ?, ?, 0)"
  val selectSql = s"SELECT * FROM $table WHERE $number = ?"

  val createSql = s"""
    CREATE TABLE $table (
      $id INTEGER PRIMARY KEY AUTOINCREMENT, $h160 STRING NOT NULL,
      $expiry INTEGER NOT NULL, $number INTEGER NOT NULL, $backup INTEGER NOT NULL
    );

    CREATE INDEX idx1$table ON $table ($number);
    CREATE INDEX idx2$table ON $table ($backup);
    COMMIT"""
}

trait Table { val (id, fts) = "_id" -> "fts4" }
class LNOpenHelper(context: Context, name: String)
  extends SQLiteOpenHelper(context, name, null, 1) {

  val base = getWritableDatabase
  def onUpgrade(dbs: SQLiteDatabase, oldVer: Int, newVer: Int) = none
  def change(sql: String, params: Any*) = base.execSQL(sql, params.map(_.toString).toArray)
  def select(sql: String, params: Any*) = base.rawQuery(sql, params.map(_.toString).toArray)
  def sqlPath(tbl: String) = Uri parse s"sqlite://com.lightning.walletapp/table/$tbl"

  def txWrap(run: => Unit) = try {
    runAnd(base.beginTransaction)(run)
    base.setTransactionSuccessful
  } finally base.endTransaction

  def onCreate(dbs: SQLiteDatabase) = {
    dbs execSQL BadEntityTable.createSql
    dbs execSQL PaymentTable.createVSql
    dbs execSQL PaymentTable.createSql
    dbs execSQL ChannelTable.createSql
    dbs execSQL OlympusTable.createSql
    dbs execSQL RevokedTable.createSql
    dbs execSQL RouteTable.createSql

    // Randomize an order of two available default servers
    val (ord1, ord2) = if (random.nextBoolean) ("0", "1") else ("1", "0")
    val emptyData = CloudData(info = None, tokens = Vector.empty, acts = Vector.empty).toJson.toString
    val dev1: Array[AnyRef] = Array("server-1", "https://a.lightning-wallet.com:9103", emptyData, "1", ord1, "0")
    val dev2: Array[AnyRef] = Array("server-2", "https://b.lightning-wallet.com:9103", emptyData, "0", ord2, "1")

    dbs.execSQL(OlympusTable.newSql, dev1)
    dbs.execSQL(OlympusTable.newSql, dev2)
  }
}