package jp.scid.specs2.jsoup

import org.jsoup.nodes.Element
import org.jsoup.select.Elements

/**
 * DOM クラスを持つ要素
 */
trait DomClassified[E] {
  /**
   * 要素にクラス値があるかを返します
   * @param element 要素
   * @param className クラス名
   * @return クラスが含まれているなら true
   */
  def haveClass(element: E, className: String): Boolean
}

private object DomClassified {
  /**
   * `Element` オブジェクト用の DOM クラス取得
   */
  implicit object ElementDomClassified extends DomClassified[Element] {
    def haveClass(element: Element, className: String) = element.hasClass(className)
  }

  /**
   * `Elements` オブジェクト用の DOM クラス取得
   */
  implicit object ElementsDomClassified extends DomClassified[Elements] {
    def haveClass(elements: Elements, className: String) = elements.hasClass(className)
  }
}
