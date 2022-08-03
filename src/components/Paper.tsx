import * as React from "react"

interface PaperProps {
  title: string
  authors: string
  year: string
  link: string
}

const Paper = ({ title, authors, year, link }: PaperProps) => {
  return (
    <section className="mt-2 pl-4">
      <h3 className="text-lg font-bold">{title}</h3>
      <p>
        {authors}. {year}.{" "}
        <a
          href={link}
          target="_blank"
          rel="noreferrer"
          className="underline text-sky-700"
        >
          Link
        </a>
      </p>
    </section>
  )
}

export default Paper
