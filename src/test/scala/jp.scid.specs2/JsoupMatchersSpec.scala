package jp.scid.specs2

import org.specs2.mutable._
import org.specs2.matcher.Matcher
import org.specs2.specification.Fragment
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

class JsoupMatchersSpec extends Specification {
  /** 検証につかう HTML */
  val testHtml = """
      <div id="element-1" class="element" title="title-1">text</div>
      <div id="element-2" class="element other-class" alt="alt-1">文字列</div>
      <div id="element-3" class="element" data-empty></div>
      <form>
        <input id="input-1" type="text" name="test-text" value="text-value">
        <input id="input-2" type="radio" name="test-radio" value="radio">
        <textarea id="textarea-element" name="test-textarea">
          text content
        </textarea>

        <select id="select-element" name="test-select">
          <option value="selected option value" selected>
        </select>

        <input type="submit" name="save">
        <input type="submit" name="delete">
        <div type="submit" name="submit-dummy">
      </form>
    """

  lazy val testDocument = Jsoup.parse(testHtml)

  "JsoupMatchers メソッド" >> {
    "#haveId(expected)" should {
      import JsoupMatchers.haveId

      "id 属性値が一致するときは検証が成功する" in {
        haveId("element-1").test(testDocument select "div" get 0) must beTrue
        haveId("element-2").test(testDocument select "div" get 1) must beTrue
      }

      "id 属性値が一致しないときは検証は成功しない" in {
        haveId("element-2").test(testDocument select "div" get 0) must beFalse
        haveId("element-1").test(testDocument select "div" get 1) must beFalse
      }
    }

    // haveClass の検証
    def haveClassExpectation(matcherGen: String => Matcher[Element]): Fragment = {
      "指定した class を含んでいるときは検証が成功する" in {
        matcherGen("element").test(testDocument select "#element-1" get 0) must beTrue
        matcherGen("element").test(testDocument select "#element-2" get 0) must beTrue
        matcherGen("other-class").test(testDocument select "#element-2" get 0) must beTrue
      }

      "指定した class が含まれていないときは検証は成功しない" in {
        matcherGen("other-class").test(testDocument select "#element-1" get 0) must beFalse
        matcherGen("element").test(testDocument select "#input-1" get 0) must beFalse
      }
    }

    "#haveClass(expected)" should haveClassExpectation(JsoupMatchers.haveClass)
    "#domClass(expected)" should haveClassExpectation(JsoupMatchers.domClass)

    "#haveAttr(attrName)" should {
      import JsoupMatchers.haveAttr

      "指定した属性値を持っているときは検証が成功する" in {
        haveAttr("title").test(testDocument select "#element-1" get 0) must beTrue
        haveAttr("id").test(testDocument select "#element-1" get 0) must beTrue
        haveAttr("alt").test(testDocument select "#element-2" get 0) must beTrue
        haveAttr("data-empty").test(testDocument select "#element-3" get 0) must beTrue
      }

      "指定した属性値を持っていないときは検証は成功しない" in {
        haveAttr("title-x").test(testDocument select "#element-1" get 0) must beFalse
        haveAttr("title").test(testDocument select "#element-2" get 0) must beFalse
      }
    }

    "#haveAttr(attrName, expected)" should {
      import JsoupMatchers.haveAttr

      "指定した属性値が一致するときは検証が成功する" in {
        haveAttr("title", "title-1").test(testDocument select "#element-1" get 0) must beTrue
        haveAttr("alt", "alt-1").test(testDocument select "#element-2" get 0) must beTrue
        haveAttr("data-empty", "").test(testDocument select "#element-3" get 0) must beTrue
      }

      "属性値が異なるときは検証は成功しない" in {
        haveAttr("title", "").test(testDocument select "#element-1" get 0) must beFalse
        haveAttr("xxxx", "title-1").test(testDocument select "#element-1" get 0) must beFalse
        haveAttr("alt", "").test(testDocument select "#element-2" get 0) must beFalse
      }
    }

    "#haveElements(query)" should {
      import JsoupMatchers.haveElements

      "クエリで選択できるときは検証を成功させる" in {
        haveElements("#element-1").test(testDocument) must beTrue
        haveElements("div.element").test(testDocument) must beTrue
      }

      "クエリで選択できないときは検証は成功しない" in {
        haveElements("#element-1").test(Jsoup parse "") must beFalse
        haveElements("p").test(testDocument) must beFalse
      }
    }

    "#haveElements(query, count)" should {
      import JsoupMatchers.haveElements

      "クエリで選択できる要素が数も一致するときに検証を成功させる" in {
        haveElements("#element-1", 1).test(testDocument) must beTrue
        haveElements("div.element", 3).test(testDocument) must beTrue
        haveElements("#element-x", 0).test(testDocument) must beTrue
      }

      "クエリで選択できても要素数が一致しないときに検証は成功しない" in {
        haveElements("#element-1", 0).test(testDocument) must beFalse
        haveElements("#element-1", 2).test(testDocument) must beFalse
        haveElements("#element-x", 1).test(testDocument) must beFalse

        haveElements("div", 0).test(testDocument) must beFalse
      }
    }

    "#haveText(expected)" should {
      import JsoupMatchers.haveText

      "文字列と一致するときは検証が成功する" in {
        haveText("text").test(testDocument select "#element-1" get 0) must beTrue
        haveText("文字列").test(testDocument select "#element-2" get 0) must beTrue
        haveText("").test(testDocument select "#element-3" get 0) must beTrue
      }

      "文字列と一致しないときは検証が成功しない" in {
        haveText("").test(testDocument select "#element-1" get 0) must beFalse
        haveText("文字列 x").test(testDocument select "#element-2" get 0) must beFalse
      }
    }

    "#haveElementWithText(query, expected)" should {
      import JsoupMatchers.haveElementWithText

      "文字列と一致するときは検証が成功する" in {
        haveElementWithText("#element-1", "text").test(testDocument) must beTrue
        haveElementWithText("#element-2", "文字列").test(testDocument) must beTrue
        haveElementWithText("#textarea-element", "text content").test(testDocument) must beTrue
      }

      "文字列が一致しないときは検証が失敗する" in {
        haveElementWithText("#element-1", "").test(testDocument) must beFalse
        haveElementWithText("#element-2", "xxx").test(testDocument) must beFalse
      }

      "クエリで複数の要素が 1 つ以外選択されるときは検証が失敗する" in {
        haveElementWithText("#element-4", "text").test(testDocument) must beFalse
        haveElementWithText("#element-2, #element-3", "文字列").test(testDocument) must beFalse
      }
    }

    "#haveVal(value)" should {
      import JsoupMatchers.haveVal

      "値が一致するときに検証が成功する" in {
        haveVal("text-value") test testDocument.select("#input-1").first must beTrue
        haveVal("radio") test testDocument.select("#input-2").first must beTrue
        haveVal("text content") test testDocument.select("#textarea-element").first must beTrue

        haveVal("") test testDocument.select("#select-element").first must beTrue
        haveVal("selected option value") test testDocument.select("#select-element option").first must beTrue
      }

      "値が一致しないときは検証は成功しない" in {
        haveVal("") test testDocument.select("#input-1").first must beFalse
        haveVal(" text-value ") test testDocument.select("#input-1").first must beFalse
      }
    }

    "#haveInputElement(name, element)" should {
      import JsoupMatchers.haveInputElement

      "指定した名前の要素が存在するときに検証が成功する" in {
        haveInputElement("test-text") test testDocument must beTrue
        haveInputElement("test-radio") test testDocument must beTrue
      }

      "名前があっていても要素が一致しないときは検証は成功しない" in {
        haveInputElement("test-textarea") test testDocument must beFalse
        haveInputElement("test-select") test testDocument must beFalse
      }
    }

    "#haveInputSubmitElement(name, element)" should {
      import JsoupMatchers.haveInputSubmitElement

      "指定した名前の要素が存在するときに検証が成功する" in {
        haveInputSubmitElement("save") test testDocument must beTrue
        haveInputSubmitElement("delete") test testDocument must beTrue
      }

      "名前があっていても type が submit の input 要素ではないときは検証は成功しない" in {
        haveInputSubmitElement("test-text") test testDocument must beFalse
        haveInputSubmitElement("submit-dummy") test testDocument must beFalse
      }
    }
  }
}
