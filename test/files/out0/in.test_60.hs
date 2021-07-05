Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'LiteralValue
                                        ( Number "1" )
                                    , values =
                                        [ Expression'LiteralValue Null
                                        , Expression'LiteralValue
                                            ( Number "1" )
                                        , Expression'LiteralValue
                                            ( Number "2" )
                                        ]
                                    }
                                )
                            , alias = Nothing
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'InValues
                                    ( InValuesExpression
                                        { expression = Expression'LiteralValue
                                            ( Number "3" )
                                        , values =
                                            [ Expression'LiteralValue Null
                                            , Expression'LiteralValue
                                                ( Number "1" )
                                            , Expression'LiteralValue
                                                ( Number "2" )
                                            ]
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InValues
                                        ( InValuesExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "1" )
                                            , values =
                                                [ Expression'LiteralValue Null
                                                , Expression'LiteralValue
                                                    ( Number "1" )
                                                , Expression'LiteralValue
                                                    ( Number "2" )
                                                ]
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        , ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Not
                                    ( Expression'InValues
                                        ( InValuesExpression
                                            { expression = Expression'LiteralValue
                                                ( Number "3" )
                                            , values =
                                                [ Expression'LiteralValue Null
                                                , Expression'LiteralValue
                                                    ( Number "1" )
                                                , Expression'LiteralValue
                                                    ( Number "2" )
                                                ]
                                            }
                                        )
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
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