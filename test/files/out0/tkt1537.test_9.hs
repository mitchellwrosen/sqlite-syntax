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
                                            , value = "t2"
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
                                            , value = "t1"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            )
                            ( Just
                                ( On
                                    ( Expression'InValues
                                        ( InValuesExpression
                                            { expression = Expression'Column
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "b"
                                                    }
                                                )
                                            , values =
                                                [ Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "a2"
                                                        }
                                                    )
                                                , Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "a1"
                                                        }
                                                    )
                                                ]
                                            }
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