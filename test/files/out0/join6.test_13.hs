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
                                                                        ( Number "3" )
                                                                    , alias = Just "b"
                                                                    }
                                                                ) :|
                                                                [ ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'LiteralValue
                                                                            ( Number "92" )
                                                                        , alias = Just "y"
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
                                                                    ( Number "4" )
                                                                , alias = Nothing
                                                                }
                                                            ) :|
                                                            [ ResultColumn'Expression
                                                                ( Aliased
                                                                    { value = Expression'LiteralValue
                                                                        ( Number "94" )
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