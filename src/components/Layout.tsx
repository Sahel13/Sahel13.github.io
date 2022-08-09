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
        <main className="max-w-screen-sm md:max-w-screen-md mx-auto px-4 mt-20">
          {children}
        </main>
      </div>
      <Footer />
    </div>
  )
}

export default Layout
