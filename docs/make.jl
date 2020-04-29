using Documenter, CovidData

makedocs(;
    modules=[CovidData],
    format=Documenter.HTML(),
    pages=[
        "Home" => "index.md",
    ],
    repo="https://github.com/schrimpf/CovidData.jl/blob/{commit}{path}#L{line}",
    sitename="CovidData.jl",
    authors="Paul Schrimpf <paul.schrimpf@gmail.com>"
         )

deploydocs(repo="github.com/schrimpf/CovidData.jl.git")
