/* References:
https://www.gatsbyjs.com/docs/reference/built-in-components/gatsby-head/#editing-html-and-body
https://www.gatsbyjs.com/docs/reference/config-files/gatsby-ssr/
*/

exports.onRenderBody = ({ setBodyAttributes }, pluginOptions) => {
  setBodyAttributes({
    className:
      "bg-slate-100 text-slate-800 max-w-screen-sm md:max-w-screen-md mx-auto",
  })
}
