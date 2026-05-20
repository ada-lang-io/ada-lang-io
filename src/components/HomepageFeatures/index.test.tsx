import React from "react"

import "@testing-library/jest-dom"
import { render, screen } from "@testing-library/react"

import HomepageFeatures from "./index"

// Mock modules before importing the component
jest.mock("@site/src/components/AlireInstallInstructions", () => {
  return {
    __esModule: true,
    default: () => <div data-testid="alire-install-instructions">Alire Install Instructions</div>
  }
})

// Mock the module styles
jest.mock("./index.module.scss", () => ({
  features: "features",
  sectionWrapper: "sectionWrapper",
  spark: "spark",
  title: "title",
  subTitle: "subTitle",
  description: "description",
  itemWrapper: "itemWrapper",
  itemIcon: "itemIcon",
  itemTitle: "itemTitle",
  itemDescription: "itemDescription"
}))

// Mock the react-icons/md components
jest.mock("react-icons/md", () => ({
  MdAutoStories: () => <span data-testid="MdAutoStories">AutoStories</span>,
  MdVerified: () => <span data-testid="MdVerified">Verified</span>,
  MdSpeed: () => <span data-testid="MdSpeed">Speed</span>,
  MdLooksOne: () => <span data-testid="MdLooksOne">LooksOne</span>,
  MdLooksTwo: () => <span data-testid="MdLooksTwo">LooksTwo</span>,
  MdLooks3: () => <span data-testid="MdLooks3">LooksThree</span>,
  MdLooks4: () => <span data-testid="MdLooks4">LooksFour</span>,
  MdLooks5: () => <span data-testid="MdLooks5">LooksFive</span>
}))

// Store the mocked JSON data in a variable first
const mockedFeaturesData = [
  {
    title: "Ada",
    subTitle: "Readable, correct, performant",
    description:
      "Express intent with explicitness, describe properties with predicates and pre/post conditions, and import C/C++ functions or intrinsics.",
    columns: 3,
    items: [
      {
        title: "Readable",
        description: [
          "Express intent with explicitness and keywords over symbols and special structures.",
          "Express concepts like meaning in integers. Use built-in design by contract with pre/post-conditions and invariants. Model problems with typechecks and range constraints."
        ],
        icon: "feat-readable"
      },
      {
        title: "Correct",
        description: [
          "Build with technology used in 40 years of reliability in planes, trains, and satellites.",
          "Use the SPARK subset to formally verify part or all of your program, and integrate existing SPARK crates available in the Alire package manager."
        ],
        icon: "feat-correct"
      },
      {
        title: "Performant",
        description: [
          "Build native applications and take advantage of other libraries through binding to C and C++.",
          "Use inline assembly or compiler intrinsics when you need it. Control resources with scope-based resource control (RAII) and your own memory allocators."
        ],
        icon: "feat-performant"
      }
    ]
  },
  {
    title: "Set-up environment",
    subTitle: "Package manager + toolchain",
    description: "Download the Alire package manager and install the compiler.",
    columns: 1
  },
  {
    title: "SPARK",
    subTitle: "From memory safety to functional correctness",
    description:
      "Gradually adopt the SPARK subset to reach various levels of assurance. Higher levels take more effort, but give more benefits and stronger guarantees.",
    items: [
      {
        title: "Validates code",
        description:
          "Restricts Ada packages to the SPARK subset. Avoids side-effects in functions and parameter aliasing.",
        icon: "spark-stone"
      },
      {
        title: "Checks initialization and data flow",
        description: "No uninitialized variables are read or undesired access to globals occurs.",
        icon: "spark-bronze"
      },
      {
        title: "Proves the absence of run-time errors",
        description:
          "No buffer and arithmetic overflow, division by zero, or values out of range, among others, can occur.",
        icon: "spark-silver"
      },
      {
        title: "Ensures key integrity properties",
        description: "Verifies integrity of data and valid state transitions.",
        icon: "spark-gold"
      }
    ]
  }
]

// Mock the JSON data
jest.mock("./features.json", () => mockedFeaturesData)

describe("HomepageFeatures", () => {
  it("renders without crashing", () => {
    render(<HomepageFeatures />)
    expect(screen.getByTestId("MdAutoStories")).toBeInTheDocument()
  })

  it("renders all feature sections", () => {
    render(<HomepageFeatures />)

    // Check that all feature titles are present
    expect(screen.getByText("Ada")).toBeInTheDocument()
    expect(screen.getByText("Set-up environment")).toBeInTheDocument()
    expect(screen.getByText("SPARK")).toBeInTheDocument()
  })

  it("renders feature descriptions correctly", () => {
    render(<HomepageFeatures />)

    // Check that main descriptions are rendered - using queryByText to avoid multiple elements
    expect(screen.queryAllByText(/Express intent with explicitness/i)).toHaveLength(2) // Found in both main and sub-descriptions
    expect(screen.getByText(/Download the Alire package manager/i)).toBeInTheDocument()
    expect(screen.getByText(/Gradually adopt the SPARK subset/i)).toBeInTheDocument()
  })

  it("renders feature items with icons", () => {
    render(<HomepageFeatures />)

    // Check that feature items with icons are rendered
    expect(screen.getByText("Readable")).toBeInTheDocument()
    expect(screen.getByText("Correct")).toBeInTheDocument()
    expect(screen.getByText("Performant")).toBeInTheDocument()

    // Check that icons are present
    expect(screen.getByTestId("MdAutoStories")).toBeInTheDocument()
    expect(screen.getByTestId("MdVerified")).toBeInTheDocument()
    expect(screen.getByTestId("MdSpeed")).toBeInTheDocument()
  })

  it("renders SPARK level descriptions", () => {
    render(<HomepageFeatures />)

    // Check SPARK feature items
    expect(screen.getByText("Validates code")).toBeInTheDocument()
    expect(screen.getByText("Checks initialization and data flow")).toBeInTheDocument()
    expect(screen.getByText("Proves the absence of run-time errors")).toBeInTheDocument()
    expect(screen.getByText("Ensures key integrity properties")).toBeInTheDocument()

    // Check SPARK icons
    expect(screen.getByTestId("MdLooksOne")).toBeInTheDocument()
    expect(screen.getByTestId("MdLooksTwo")).toBeInTheDocument()
    expect(screen.getByTestId("MdLooks3")).toBeInTheDocument()
    expect(screen.getByTestId("MdLooks4")).toBeInTheDocument()
  })

  it("renders Alire install instructions component", () => {
    render(<HomepageFeatures />)

    // Check that the Alire install instructions component is rendered in the second section
    expect(screen.getByTestId("alire-install-instructions")).toBeInTheDocument()
  })
})
