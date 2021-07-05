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
                        ( Table'InnerJoin
                            ( Table
                                ( QualifiedTableName
                                    { name = Aliased
                                        { value = Namespaced
                                            { namespace = Nothing
                                            , value = "aa"
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
                                            , value = "bb"
                                            }
                                        , alias = Nothing
                                        }
                                    , indexedBy = Nothing
                                    }
                                )
                            ) Nothing
                        )
                    , where_ = Just
                        ( Expression'FunctionCall
                            ( FunctionCallExpression
                                { call = FunctionCall
                                    { name = Namespaced
                                        { namespace = Nothing
                                        , value = "min"
                                        }
                                    , arguments = FunctionArguments'Arguments
                                        [ Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "a"
                                                }
                                            )
                                        , Expression'Column
                                            ( Namespaced
                                                { namespace = Nothing
                                                , value = "b"
                                                }
                                            )
                                        ]
                                    }
                                , filter = Nothing
                                , over = Nothing
                                }
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