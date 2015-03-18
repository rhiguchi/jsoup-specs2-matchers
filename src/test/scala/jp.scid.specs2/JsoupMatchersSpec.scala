package jp.scid.specs2

import org.specs2.mutable._
import org.jsoup.Jsoup

class JsoupMatchersSpec extends Specification {
  /** 検証につかう HTML */
  val testHtml = """
      <div id="element-1" class="elemnet">text</div>
      <div id="element-2" class="elemnet">文字列</div>
      <div id="element-3" class="elemnet"></div>
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
    "#haveElements(query)" should {
      import JsoupMatchers.haveElements

      "クエリで選択できるときは検証を成功させる" in {
        haveElements("#element-1").test(testDocument) must beTrue
        haveElements("div.elemnet").test(testDocument) must beTrue
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
        haveElements("div.elemnet", 3).test(testDocument) must beTrue
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
        haveElementWithText("#element-1", "").test(testDocument) must beTrue
        haveElementWithText("#element-2", "xxx").test(testDocument) must beTrue
      }

      "クエリで複数の要素が 1 つ以外選択されるときは検証が失敗する" in {
        haveElementWithText("#element-4", "text").test(testDocument) must beFalse
        haveElementWithText("#element-2, #element-3", "文字列").test(testDocument) must beFalse
      }

      // "文字列と一致しないときは検証が成功しない" in {
      //   haveText("").test(testDocument select "#element-1" get 0) must beFalse
      //   haveText("文字列 x").test(testDocument select "#element-2" get 0) must beFalse
      // }
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
