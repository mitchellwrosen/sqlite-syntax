Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t1"
                }
            , alias = Nothing
            }
        , columns = Nothing
        , insert = InsertSelect
            ( SelectStatement
                { commonTableExpressions = Nothing
                , select = CompoundSelect
                    ( SelectCore'Select
                        ( Select
                            { distinct = False
                            , columns = ResultColumn'Expression
                                ( Aliased
                                    { value = Expression'Plus
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        )
                                        ( Expression'LiteralValue
                                            ( Number "10" )
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
                                ( Expression'LessThan
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "a"
                                            }
                                        )
                                    )
                                    ( Expression'LiteralValue
                                        ( Number "6" )
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
            ) Nothing
        , returning = Nothing
        }
    )