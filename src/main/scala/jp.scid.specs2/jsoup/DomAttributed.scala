package jp.scid.specs2.jsoup

import org.jsoup.nodes.Element
import org.jsoup.select.Elements

/**
 * DOM 属性を持つ要素
 */
trait DomAttributed[E] {
  /**
   * 要素にクラス値があるかを返します
   * @param element 要素
   * @param attrName 属性名
   * @return 属性が含まれているなら true
   */
  def haveAttr(element: E, attrName: String): Boolean
}

private object DomAttributed {
  /**
   * `Element` オブジェクト用の DOM クラス取得
   */
  implicit object ElementDomAttributed extends DomAttributed[Element] {
    def haveAttr(element: Element, attrName: String) = element.hasAttr(attrName)
  }

  /**
   * `Elements` オブジェクト用の DOM クラス取得
   */
  implicit object ElementsDomAttributed extends DomAttributed[Elements] {
    def haveAttr(elements: Elements, attrName: String) = elements.hasAttr(attrName)
  }
}
