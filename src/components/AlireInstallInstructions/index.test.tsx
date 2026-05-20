import React from "react"

import "@testing-library/jest-dom"
import { fireEvent, render, screen } from "@testing-library/react"

import AlireInstallInstructions from "./index"

// Mock the SCSS module
jest.mock("./index.module.scss", () => ({
  timelineItemTitle: "timelineItemTitle"
}))

// Mock the Mantine components
jest.mock("@mantine/core", () => {
  const TimelineItem = ({
    children,
    title,
    bullet,
    ...props
  }: {
    children: React.ReactNode
    title: React.ReactNode
    bullet: React.ReactNode
    [key: string]: any
  }) => (
    <div data-testid="timeline-item" {...props}>
      <div data-testid="timeline-bullet">{bullet}</div>
      <div data-testid="timeline-title">{title}</div>
      <div>{children}</div>
    </div>
  )

  const Timeline = ({
    children,
    active,
    bulletSize,
    lineWidth,
    ...props
  }: {
    children: React.ReactNode
    active?: number
    bulletSize?: number
    lineWidth?: number
    [key: string]: any
  }) => {
    return (
      <div data-testid="timeline" data-active={active} {...props}>
        {children}
      </div>
    )
  }

  // Assign TimelineItem as a static property of Timeline
  ;(Timeline as any).Item = TimelineItem

  return {
    Timeline,
    TimelineItem,
    Stack: ({ children, ...props }: { children: React.ReactNode; [key: string]: any }) => (
      <div data-testid="stack" {...props}>
        {children}
      </div>
    ),
    Text: ({
      children,
      color,
      ...props
    }: {
      children: React.ReactNode
      color?: string
      [key: string]: any
    }) => (
      <span data-testid="text" style={{ color }} {...props}>
        {children}
      </span>
    )
  }
})

// Mock the Mantine prism component
jest.mock("@mantine/prism", () => {
  const React = require("react")
  const { forwardRef } = React
  return {
    Prism: forwardRef(
      (
        props: { children: React.ReactNode; language?: string; [key: string]: any },
        ref: React.Ref<any>
      ) => {
        const { children, language, ...rest } = props
        return (
          <pre ref={ref} data-testid="prism" data-language={language} {...rest}>
            {children}
          </pre>
        )
      }
    )
  }
})

// Mock the Mantine hooks
jest.mock("@mantine/hooks", () => ({
  useOs: () => "linux",
  useEventListener: jest.fn((_, handler) => () => {})
}))

// Mock react-icons
jest.mock("react-icons/fa", () => ({ FaTerminal: () => <span data-testid="fa-terminal" /> }))
jest.mock("react-icons/md", () => ({
  MdCode: () => <span data-testid="md-code" />,
  MdDone: () => <span data-testid="md-done" />,
  MdFileDownload: () => <span data-testid="md-file-download" />
}))

// Mock Docusaurus modules
jest.mock("@docusaurus/Link", () => ({
  __esModule: true,
  default: ({
    children,
    to,
    onClick,
    href,
    ...props
  }: {
    children: React.ReactNode
    to?: string
    onClick?: () => void
    href?: string
    [key: string]: any
  }) => (
    <a href={to || href} onClick={onClick} {...props}>
      {children}
    </a>
  )
}))

jest.mock("@docusaurus/useGlobalData", () => ({
  usePluginData: jest.fn(() => ({ alireVersion: "2.0.0" }))
}))

jest.mock("@docusaurus/useIsBrowser", () => () => true)

// Mock the page utility functions
jest.mock("@site/src/pages/index", () => ({
  getInstallTarget: jest.fn(
    (version, suffix) =>
      `https://github.com/alire-project/alire/releases/download/${version}/alr-${version.slice(
        1
      )}-${suffix}`
  ),
  gitHubReleasePage: "https://github.com/alire-project/alire/releases",
  installTargets: new Map([
    ["linux", { label: "Linux", urlSuffix: "bin-x86_64-linux.zip" }],
    ["macos", { label: "macOS", urlSuffix: "bin-universal-macos.zip" }],
    ["windows", { label: "Windows", urlSuffix: "installer-x86_64-windows.exe" }]
  ])
}))

describe("AlireInstallInstructions", () => {
  beforeEach(() => {
    jest.clearAllMocks()
  })

  it("renders the component with timeline steps", () => {
    render(<AlireInstallInstructions />)

    expect(screen.getByTestId("timeline")).toBeInTheDocument()
    expect(screen.getAllByTestId("timeline-item")).toHaveLength(4)

    // Check for the presence of the timeline titles
    expect(screen.getByText("Download Alire")).toBeInTheDocument()
    expect(screen.getByText("Install toolchain")).toBeInTheDocument()
    expect(screen.getByText("Start coding")).toBeInTheDocument()
    expect(screen.getByText("Run your application")).toBeInTheDocument()
  })

  it("displays the correct OS information when platform is detected", () => {
    const mockHooks = jest.requireMock("@mantine/hooks")
    jest.spyOn(mockHooks, "useOs").mockReturnValue("linux")

    render(<AlireInstallInstructions />)

    // Should show Linux-specific instructions
    expect(screen.getByText(/Alire 2\.0\.0/)).toBeInTheDocument()
  })

  it("handles unsupported OS gracefully", () => {
    const mockHooks = jest.requireMock("@mantine/hooks")
    jest.spyOn(mockHooks, "useOs").mockReturnValue("android") // Android not in installTargets

    render(<AlireInstallInstructions />)

    // Should not show platform-specific version info
    expect(screen.queryByText(/Alire 2\.0\.0.*Android/)).not.toBeInTheDocument()
  })

  it("updates the active step when clicking on download link", () => {
    render(<AlireInstallInstructions />)

    // The text is split across elements, so we look for the link with the text "Alire 2.0.0"
    const downloadLink = screen.getByRole("link", { name: /Alire 2\.0\.0/ })
    fireEvent.click(downloadLink)

    const timeline = screen.getByTestId("timeline")
    expect(timeline).toHaveAttribute("data-active", "0")
  })

  it("displays platform-specific download instructions for macOS", () => {
    const mockHooks = jest.requireMock("@mantine/hooks")
    jest.spyOn(mockHooks, "useOs").mockReturnValue("macos")

    render(<AlireInstallInstructions />)

    // Should show macOS-specific instructions
    expect(screen.getByText(/remove the quarantine attribute/)).toBeInTheDocument()
  })

  it("has the correct bash commands in code blocks", () => {
    render(<AlireInstallInstructions />)

    expect(screen.getByText("alr toolchain --select")).toBeInTheDocument()
    expect(screen.getByText("alr init --bin mycrate && cd mycrate")).toBeInTheDocument()
    expect(screen.getByText("alr build")).toBeInTheDocument()
    expect(screen.getByText("alr run")).toBeInTheDocument()
  })
})
