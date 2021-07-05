Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Wildcard
                        ( Namespaced
                            { namespace = Nothing
                            , value = ()
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
                            ( Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "x"
                                            }
                                        )
                                    , values =
                                        [ Expression'LiteralValue
                                            ( Number "1" )
                                        , Expression'LiteralValue
                                            ( Number "2" )
                                        ]
                                    }
                                )
                            )
                            ( Expression'InValues
                                ( InValuesExpression
                                    { expression = Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "y"
                                            }
                                        )
                                    , values =
                                        [ Expression'Negate
                                            ( Expression'LiteralValue
                                                ( Number "5" )
                                            )
                                        , Expression'Negate
                                            ( Expression'LiteralValue
                                                ( Number "6" )
                                            )
                                        ]
                                    }
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