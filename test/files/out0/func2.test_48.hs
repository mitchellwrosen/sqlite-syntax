Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "SUBSTR"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'LiteralValue
                                                ( String "Supercalifragilisticexpialidocious" )
                                            , Expression'LiteralValue
                                                ( Number "2" )
                                            , Expression'Negate
                                                ( Expression'LiteralValue
                                                    ( Number "2" )
                                                )
                                            ]
                                        }
                                    , filter = Nothing
                                    , over = Nothing
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :| []
                    , from = Nothing
                    , where_ = Nothing
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )