package jp.scid.specs2

import scala.collection.JavaConverters._

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.specs2.matcher.Matcher

/**
 * Jsoup (http://jsoup.org) のオブジェクトの検証メソッド集
 */
object JsoupMatchers extends jsoup.ElementMatchers
