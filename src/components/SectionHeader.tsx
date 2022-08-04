import * as React from "react"

interface HeaderProps {
  children: React.ReactNode
}

const SectionHeader = ({ children }: HeaderProps) => {
  return (
    <>
      <h2 className="text-3xl font-bold mb-2">{children}</h2>
      <div className="h-1 bg-black mb-4"></div>
    </>
  )
}

export default SectionHeader
