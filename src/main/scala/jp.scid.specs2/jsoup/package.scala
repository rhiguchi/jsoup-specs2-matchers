package jp.scid.specs2

package object jsoup {
  /** マッチャーをミックインせずに参照するためのインスタンスオブジェクト */
  private[jsoup] object Specs2Matchers extends org.specs2.matcher.Matchers
}
