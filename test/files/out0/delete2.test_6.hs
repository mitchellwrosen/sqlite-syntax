Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Case
                                ( CaseExpression
                                    { base = Nothing
                                    , cases =
                                        ( Expression'Equals
                                            ( Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "c"
                                                    }
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "5" )
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
                                                }
                                            )
                                        ) :| []
                                    , else_ = Expression'LiteralValue Null
                                    }
                                )
                            , alias = Just "b"
                            }
                        ) :|
                        [ ResultColumn'Expression
                            ( Aliased
                                { value = Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "c"
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
                                        , value = "d"
                                        }
                                    )
                                , alias = Nothing
                                }
                            )
                        ]
                    , from = Just
                        ( Table'InnerJoin
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
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "t2"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            ) Nothing
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