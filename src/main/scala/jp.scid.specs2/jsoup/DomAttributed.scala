package jp.scid.specs2.jsoup

import org.jsoup.nodes.Element
import org.jsoup.select.Elements

/**
 * DOM 属性を持つ要素
 */
trait DomAttributed[E] {
  /**
   * 要素に属性値があるかを返します
   * @param element 要素
   * @param attrName 属性名
   * @return 属性が含まれているなら true
   */
  def haveAttr(element: E, attrName: String): Boolean

  /**
   * 要素の属性値を返します
   * @param element 要素
   * @param attrName 属性名
   * @return 属性値
   */
  def attr(element: E, attrName: String): String
}

private object DomAttributed {
  /**
   * `Element` オブジェクト用の DOM クラス取得
   */
  implicit object ElementDomAttributed extends DomAttributed[Element] {
    def haveAttr(element: Element, attrName: String) = element.hasAttr(attrName)

    def attr(element: Element, attrName: String) = element.attr(attrName)
  }

  /**
   * `Elements` オブジェクト用の DOM クラス取得
   */
  implicit object ElementsDomAttributed extends DomAttributed[Elements] {
    def haveAttr(elements: Elements, attrName: String) = elements.hasAttr(attrName)

    def attr(elements: Elements, attrName: String) = elements.attr(attrName)
  }
}
