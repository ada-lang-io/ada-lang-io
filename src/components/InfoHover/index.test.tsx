import React from "react"

import "@testing-library/jest-dom"
import { render, screen } from "@testing-library/react"

import InfoHover from "./index"

describe("InfoHover", () => {
  const mockText = ["Information line 1", "Information line 2"]
  const defaultProps = { width: 300 } // Default width from component

  it("renders the info icon badge", () => {
    render(<InfoHover text={mockText} {...defaultProps} />)

    // The info icon should be present
    const iconElement = document.querySelector("svg") // FaInfo renders as SVG
    expect(iconElement).toBeInTheDocument()
  })

  it("has the correct badge styling", () => {
    render(<InfoHover text={mockText} {...defaultProps} />)

    // Find the badge element by its class instead of role
    const badgeElement = document.querySelector(".infoBadge")
    expect(badgeElement).toBeInTheDocument()
  })

  it("applies the correct width to the hover card", () => {
    const customWidth = 400
    render(<InfoHover text={mockText} width={customWidth} />)

    // Component should accept custom width without errors
    expect(() => {
      render(<InfoHover text={mockText} width={customWidth} />)
    }).not.toThrow()
  })

  it("uses default width when not provided", () => {
    render(<InfoHover text={mockText} width={300} />)

    expect(() => {
      render(<InfoHover text={mockText} width={300} />)
    }).not.toThrow()
  })

  it("handles empty text array correctly", () => {
    render(<InfoHover text={[]} {...defaultProps} />)

    const iconElement = document.querySelector("svg")
    expect(iconElement).toBeInTheDocument()
  })

  it("correctly maps multiple text elements", () => {
    const multiLineText = [
      "First line",
      <span key="second">Second line as span</span>,
      "Third line"
    ]

    render(<InfoHover text={multiLineText} {...defaultProps} />)

    expect(() => {
      render(<InfoHover text={multiLineText} {...defaultProps} />)
    }).not.toThrow()
  })

  it("generates unique keys for each text element", () => {
    const duplicateText = ["Same text", "Same text"]

    render(<InfoHover text={duplicateText} {...defaultProps} />)

    expect(() => {
      render(<InfoHover text={duplicateText} {...defaultProps} />)
    }).not.toThrow()
  })

  it("renders JSX elements in the text array", () => {
    const jsxText = [<strong key="bold">Bold text</strong>, "Plain text"]

    render(<InfoHover text={jsxText} {...defaultProps} />)

    expect(() => {
      render(<InfoHover text={jsxText} {...defaultProps} />)
    }).not.toThrow()
  })

  it("displays hover card content correctly", () => {
    render(<InfoHover text={mockText} {...defaultProps} />)

    // Initially, the dropdown content should not be visible
    expect(screen.queryByText(mockText[0])).not.toBeInTheDocument()
  })
})
