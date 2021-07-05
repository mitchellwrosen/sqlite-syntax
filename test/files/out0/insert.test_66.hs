Statement'Insert
    ( InsertStatement
        { commonTableExpressions = Nothing
        , onConflict = Abort
        , table = Aliased
            { value = Namespaced
                { namespace = Nothing
                , value = "t3"
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
                            , columns = ResultColumn'Wildcard
                                ( Namespaced
                                    { namespace = Nothing
                                    , value = ()
                                    }
                                ) :| []
                            , from = Just
                                ( Table'Subquery
                                    ( Aliased
                                        { value = SelectStatement
                                            { commonTableExpressions = Nothing
                                            , select = UnionAll
                                                ( CompoundSelect
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
                                                                                , value = "t3"
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
                                                )
                                                ( SelectCore'Select
                                                    ( Select
                                                        { distinct = False
                                                        , columns = ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'LiteralValue
                                                                    ( Number "1" )
                                                                , alias = Nothing
                                                                }
                                                            ) :|
                                                            [ ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "2" )
                                                                    , alias = Nothing
                                                                    }
                                                                )
                                                            , ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "3" )
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
                                        , alias = Nothing
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
            ) Nothing
        , returning = Nothing
        }
    )