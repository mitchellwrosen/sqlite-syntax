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
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'InValues
                            ( InValuesExpression
                                { expression = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "rowid"
                                        }
                                    )
                                , values =
                                    [ Expression'Negate
                                        ( Expression'LiteralValue
                                            ( Number "1" )
                                        )
                                    , Expression'LiteralValue
                                        ( Number "2" )
                                    , Expression'LiteralValue
                                        ( Number "4" )
                                    ]
                                }
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