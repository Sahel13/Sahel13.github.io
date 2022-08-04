import * as React from "react"
import { Link } from "gatsby"

const NavigationBar = () => {
  return (
    <nav>
      <h1 className="font-bold text-xl">
        <Link to="/">Sahel Mohammad Iqbal</Link>
      </h1>
    </nav>
  )
}

export default NavigationBar
