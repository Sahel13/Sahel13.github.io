import * as React from "react"
import { Link } from "gatsby"
import { FaBars, FaTimes } from "react-icons/fa"

const NavigationBar = () => {
  const [isOpen, setIsOpen] = React.useState(false)

  const toggle = () => {
    setIsOpen(!isOpen)
  }

  return (
    <nav className="flex justify-between relative">
      <h1 className="font-bold text-xl my-6 mx-4">
        <Link to="/">Sahel Mohammad Iqbal</Link>
      </h1>
      <div
        onClick={toggle}
        className="cursor-pointer flex items-center z-20 my-6 mx-4"
      >
        {isOpen ? (
          <FaTimes className="text-5xl" />
        ) : (
          <FaBars className="text-xl" />
        )}
      </div>
      <div
        onClick={toggle}
        className={
          "absolute bg-slate-400 h-screen w-screen grid grid-cols-1 place-content-center z-10 " +
          (isOpen
            ? "ease-in duration-300"
            : "transform -translate-y-full ease-out duration-300")
        }
      >
        {NavbarLinks.map((navitem) => (
          <a
            key={navitem.title}
            href={navitem.address}
            className="text-center text-2xl py-3"
          >
            {navitem.title}
          </a>
        ))}
      </div>
    </nav>
  )
}

const NavbarLinks = [
  {
    title: "About",
    address: "#about",
  },
  {
    title: "Preprints",
    address: "#preprints",
  },
]

export default NavigationBar
