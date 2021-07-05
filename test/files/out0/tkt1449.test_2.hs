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
                                    , value = "NEWENTITIES"
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
                                        , value = "ITEMS"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'And
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "ISSUEID"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( String "x" )
                                )
                            )
                            ( Expression'Equals
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "OBJECTID"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( String "y" )
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