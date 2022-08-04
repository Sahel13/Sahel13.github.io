import * as React from "react"
import type { HeadFC } from "gatsby"
import Layout from "../components/Layout"
import Paper from "../components/Paper"
import SectionHeader from "../components/SectionHeader"
import Seo from "../components/Seo"
import { StaticImage } from "gatsby-plugin-image"
import { preprints } from "../data/preprints"

const IndexPage = () => {
  return (
    <Layout>
      <section id="about" className="my-6">
        <SectionHeader>About</SectionHeader>
        <div className="md:grid md:grid-cols-5 md:gap-6">
          <StaticImage
            src="../images/profile_picture.jpg"
            alt="My profile picture."
            loading="eager"
            className="md:col-span-2 md:flex md:items-center"
            objectFit="cover"
          />
          <div className="md:col-span-3 md:py-4">
            <p className="text-lg pt-8 md:pt-0">
              I am an incoming PhD student at the Department of Electrical
              Engineering and Automation at Aalto University, Finland. I
              recently graduated from the National Institute of Science
              Education and Research (NISER), Bhubaneswar with an Integrated
              Master's degree in physics.
            </p>
            <p className="text-lg pt-4">
              For academic details, see my{" "}
              <a
                href="/cv_sahelmiqbal.pdf"
                target="_blank"
                rel="noreferrer"
                className="underline text-sky-700"
              >
                resume
              </a>{" "}
              (last updated on 1st July).
            </p>
          </div>
        </div>
      </section>
      <section id="preprints" className="my-6">
        <SectionHeader>Preprints</SectionHeader>
        {preprints.map((element) => (
          <Paper
            key={element.title}
            title={element.title}
            authors={element.authors}
            year={element.year}
            link={element.link}
          />
        ))}
      </section>
    </Layout>
  )
}

export default IndexPage

export const Head: HeadFC = () => {
  return <Seo />
}
