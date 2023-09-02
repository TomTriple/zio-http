package zio.http.ttt

trait Elements {

    val div:Html.Node.Name = Html.Node.Name("div")

    val ul:Html.Node.Name = Html.Node.Name("ul")

    val li:Html.Node.Name = Html.Node.Name("li")

    val a:Html.Node.Name = Html.Node.Name("a")

    val hrefAttr:Html.Attribute.Name[String] = Html.Attribute.Name("href")

    val classAttr:Html.Attribute.Name[List[String]] = Html.Attribute.Name("class")

}