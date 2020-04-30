using CovidData, DataFrames
using Test

@testset "CovidData.jl" begin
  df = CovidData.covidjhudata()
  @test isa(df, DataFrame)
  df = CovidData.statedata()
  @test isa(df, DataFrame)
  df = CovidData.statedata(fillmissingmobility=true)
  @test isa(df, DataFrame)
  df = CovidData.statedata(policies=:indicators)
  @test isa(df, DataFrame)
end
