Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Minus
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "total"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "x"
                                                        }
                                                    )
                                                ]
                                            }
                                        , filter = Nothing
                                        , over = Nothing
                                        }
                                    )
                                )
                                ( Expression'Plus
                                    ( Expression'Multiply
                                        ( Expression'ShiftLeft
                                            ( Expression'LiteralValue
                                                ( Number "1" )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "62" )
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "2.0" )
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "1" )
                                    )
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
                                        , value = "t6"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
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