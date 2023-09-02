package zio.http.tpl

trait Elements {

    def div: Html.Element.Name = Html.Element.Name("div")

    def ul: Html.Element.Name = Html.Element.Name("ul")

    def li: Html.Element.Name = Html.Element.Name("li")

    def a: Html.Element.Name = Html.Element.Name("li")

    def hrefAttr = Html.Attribute.Name[String]("href")

    def classAttr = Html.Attribute.Name[List[String]]("class")



}