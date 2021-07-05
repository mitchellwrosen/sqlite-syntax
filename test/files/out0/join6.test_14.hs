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
                        ( Table'NaturalInnerJoin
                            ( Table'NaturalInnerJoin
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
                                )
                            )
                            ( Table'Subquery
                                ( Aliased
                                    { value = SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = Union
                                            ( CompoundSelect
                                                ( SelectCore'Select
                                                    ( Select
                                                        { distinct = False
                                                        , columns = ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'LiteralValue
                                                                    ( Number "5" )
                                                                , alias = Just "c"
                                                                }
                                                            ) :|
                                                            [ ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "91" )
                                                                    , alias = Just "x"
                                                                    }
                                                                )
                                                            , ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "93" )
                                                                    , alias = Just "z"
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
                                            )
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'LiteralValue
                                                                ( Number "6" )
                                                            , alias = Nothing
                                                            }
                                                        ) :|
                                                        [ ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'LiteralValue
                                                                    ( Number "99" )
                                                                , alias = Nothing
                                                                }
                                                            )
                                                        , ResultColumn'Expression
                                                            ( Aliased
                                                                { value = Expression'LiteralValue
                                                                    ( Number "95" )
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