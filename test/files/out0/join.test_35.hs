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
                        ( Table'LeftOuterJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t9"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
                            ( Table'Subquery
                                ( Aliased
                                    { value = SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = CompoundSelect
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'LiteralValue
                                                                ( Number "44" )
                                                            , alias = Nothing
                                                            }
                                                        ) :|
                                                        [ ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "p"
                                                                        }
                                                                    )
                                                                , alias = Nothing
                                                                }
                                                            )
                                                        , ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "q"
                                                                        }
                                                                    )
                                                                , alias = Nothing
                                                                }
                                                            )
                                                        ]
                                                    , from = Just
                                                        ( Table
                                                            ( QualifiedTableName
                                                                { name = Aliased
                                                                    { value = Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "t11"
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
                                        , orderBy = Nothing
                                        , limit = Nothing
                                        }
                                    , alias = Just "sub1"
                                    }
                                )
                            )
                            ( Just
                                ( On
                                    ( Expression'Equals
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "p"
                                                }
                                            )
                                        )
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        )
                                    )
                                )
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
    )