/* References:
https://www.gatsbyjs.com/docs/reference/built-in-components/gatsby-head/#editing-html-and-body
https://www.gatsbyjs.com/docs/reference/config-files/gatsby-ssr/
*/

exports.onRenderBody = (
  { setHtmlAttributes, setBodyAttributes },
  pluginOptions
) => {
  setHtmlAttributes({
    className: "scroll-smooth",
  })
  setBodyAttributes({
    className: "bg-slate-100 text-slate-800",
  })
}
