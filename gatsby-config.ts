import type { GatsbyConfig } from "gatsby"

const config: GatsbyConfig = {
  siteMetadata: {
    title: `Sahel Mohammad Iqbal`,
    description: `Academic portfolio website of Sahel Mohammad Iqbal.`,
    siteUrl: `https://sahel13.github.io`,
    image: `/profile_picture.jpg`,
    twitterUsername: `@sahel_iqbal`,
    author: `Sahel Mohammad Iqbal`,
  },
  // More easily incorporate content into your pages through automatic TypeScript type generation and better GraphQL IntelliSense.
  // If you use VSCode you can also use the GraphQL plugin
  // Learn more at: https://gatsby.dev/graphql-typegen
  graphqlTypegen: true,
  plugins: [
    `gatsby-plugin-image`,
    `gatsby-plugin-sharp`,
    `gatsby-transformer-sharp`,
    `gatsby-plugin-postcss`,
  ],
}

export default config
