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
                                                                        ( Number "1" )
                                                                    , alias = Just "a"
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
                                                                            ( Number "92" )
                                                                        , alias = Just "y"
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
                                                                    ( Number "2" )
                                                                , alias = Nothing
                                                                }
                                                            ) :|
                                                            [ ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "93" )
                                                                    , alias = Nothing
                                                                    }
                                                                )
                                                            , ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "94" )
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