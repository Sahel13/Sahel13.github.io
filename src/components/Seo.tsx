import * as React from "react"
import { useSiteMetadata } from "../hooks/use-site-metadata"

interface SeoProps {
  title: string
  description: string
  pathname: string
  children: React.ReactNode
}

const Seo = ({ title, description, pathname, children }: SeoProps) => {
  const {
    title: defaultTitle,
    description: defaultDescription,
    image,
    siteUrl,
    twitterUsername,
    author,
  } = useSiteMetadata()

  const seo = {
    title: title || defaultTitle,
    description: description || defaultDescription,
    image: `${siteUrl}${image}`,
    url: `${siteUrl}${pathname || ``}`,
    twitterUsername,
    author,
  }

  return (
    <>
      {/* General */}
      <title>{seo.title}</title>
      <meta name="description" content={seo.description} />
      <meta name="image" content={seo.image} />
      <meta name="author" content={seo.author} />
      <link rel="canonical" href={seo.url} />

      {/* Open Graph */}
      <meta property="og:title" content={seo.title} />
      <meta property="og:type" content={"website"} />
      <meta property="og:url" content={seo.url} />
      <meta property="og:image" content={seo.image} />
      <meta property="og:image:alt" content={"Sahel Mohammad Iqbal."} />
      <meta property="og:description" content={seo.description} />

      {/* Twitter */}
      <meta name="twitter:card" content={"summary"} />
      <meta name="twitter:title" content={seo.title} />
      <meta name="twitter:site" content={seo.twitterUsername} />
      <meta name="twitter:url" content={seo.url} />
      <meta name="twitter:description" content={seo.description} />
      <meta name="twitter:image" content={seo.image} />
      <meta name="twitter:image:alt" content={"Sahel Mohammad Iqbal."} />
      <meta name="twitter:creator" content={seo.twitterUsername} />

      {children}
    </>
  )
}

export default Seo
