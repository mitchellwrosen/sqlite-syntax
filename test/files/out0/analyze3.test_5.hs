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
                                            , value = "sum"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "y"
                                                    }
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
                    , from = Just
                        ( Table
                            ( QualifiedTableName
                                { name = Aliased
                                    { value = Namespaced
                                        { namespace = Nothing
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'GreaterThan
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "x"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "200" )
                                )
                            )
                            ( Expression'LessThan
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "x"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "300" )
                                )
                            )
                        )
                    , groupBy = Nothing
                    , window = Nothing
                    }
                )
            )
        , orderBy = Nothing
        , limit = Nothing
        }
    )