import React from "react"

import { render, screen } from "@testing-library/react"

import MarginText from "./index"

describe("MarginText", () => {
  it("should render nothing when no children are provided", () => {
    const { container } = render(<MarginText>{""}</MarginText>)
    expect(container.firstChild).toBeNull()
  })

  it("should render anchor element with correctly formatted id when children are provided", () => {
    render(<MarginText>{"section/1_subclause"}</MarginText>)

    const linkElement = screen.getByRole("link")
    expect(linkElement).toBeInTheDocument()
    expect(linkElement.getAttribute("id")).toBe("psection_1_subclause")
    expect(linkElement.getAttribute("href")).toBe("#psection_1_subclause")
  })

  it("should render only the part before the first underscore in the children", () => {
    render(<MarginText>{"section/1_subclause_part2"}</MarginText>)

    expect(screen.getByText("section/1")).toBeInTheDocument()
  })

  it("should handle children without underscores correctly", () => {
    render(<MarginText>{"simple_section"}</MarginText>)

    expect(screen.getByText("simple")).toBeInTheDocument()
  })

  it("should handle children with forward slashes correctly", () => {
    render(<MarginText>{"part/one_subsection"}</MarginText>)

    expect(screen.getByText("part/one")).toBeInTheDocument()
  })

  it("should apply the correct CSS class", () => {
    const { container } = render(<MarginText>{"test/content"}</MarginText>)

    const linkElement = container.firstChild as HTMLElement
    expect(linkElement).toHaveAttribute("class")
    expect(linkElement.getAttribute("class")).toBeTruthy()
  })
})
