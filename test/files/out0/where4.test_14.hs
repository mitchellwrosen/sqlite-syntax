Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Column
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = "rowid"
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
                                        , value = "t4"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'And
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
                                                ( Number "9" )
                                            , Expression'LiteralValue Null
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
                                            [ Expression'LiteralValue
                                                ( Number "1" )
                                            , Expression'LiteralValue
                                                ( Number "3" )
                                            , Expression'LiteralValue
                                                ( Number "2" )
                                            ]
                                        }
                                    )
                                )
                            )
                            ( Expression'NotEquals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "z"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( Number "13" )
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