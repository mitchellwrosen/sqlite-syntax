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
                                        , value = "av2"
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
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "a"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "av2 a" )
                                    )
                                )
                                ( Expression'Equals
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "b"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( String "av2 b" )
                                    )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "c"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( String "av2 c" )
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