import * as React from "react"
import Header from "./Header"
import Footer from "./Footer"
import "../styles/global.css"

interface LayoutProps {
  children: React.ReactNode
}

const Layout = ({ children }: LayoutProps) => {
  return (
    <div className="flex flex-col min-h-screen">
      <div className="grow">
        <Header />
        <main className="mx-4">{children}</main>
      </div>
      <Footer />
    </div>
  )
}

export default Layout
