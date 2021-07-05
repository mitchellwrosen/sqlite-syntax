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
                                        , value = "e"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'Match
                            ( Expression'Concatenate
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "rowid"
                                        }
                                    )
                                )
                                ( Expression'LiteralValue
                                    ( String "" )
                                )
                            )
                            ( Expression'LiteralValue
                                ( String "pattern" )
                            ) Nothing
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