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
                                        , value = "echo_c"
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
                                    [ Expression'LiteralValue
                                        ( Number "1" )
                                    , Expression'LiteralValue
                                        ( Number "2" )
                                    , Expression'LiteralValue
                                        ( Number "3" )
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