import * as React from "react"
import { Link } from "gatsby"
import { FaBars, FaTimes } from "react-icons/fa"

const Header = () => {
  const [isOpen, setIsOpen] = React.useState(false)

  const toggle = () => {
    setIsOpen(!isOpen)
  }

  return (
    <header className="fixed z-10 w-full bg-slate-100">
      <nav className="max-w-screen-sm md:max-w-screen-md mx-auto flex justify-between items-center h-20">
        <h1 className="font-bold text-xl px-4">
          <Link to="/">Sahel Mohammad Iqbal</Link>
        </h1>
        <div onClick={toggle} className="cursor-pointer z-20 px-4 md:hidden">
          {isOpen ? (
            <FaTimes className="text-5xl" />
          ) : (
            <FaBars className="text-xl" />
          )}
        </div>
        <div
          onClick={toggle}
          className={
            "absolute top-0 left-0 bg-slate-300 h-screen w-screen grid grid-cols-1 place-content-center z-10 transition-transform " +
            (isOpen
              ? "ease-in duration-300"
              : "transform -translate-y-full ease-out duration-300") +
            " md:static md:transform-none md:w-auto md:h-auto md:bg-inherit md:block"
          }
        >
          {NavbarLinks.map((navitem) => (
            <a
              key={navitem.title}
              href={navitem.address}
              className="text-center text-2xl py-3 md:text-xl md:py-0 md:px-4"
            >
              {navitem.title}
            </a>
          ))}
        </div>
      </nav>
    </header>
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

export default Header
