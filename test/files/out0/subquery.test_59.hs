Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Subquery
                                ( SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = CompoundSelect
                                        ( SelectCore'Select
                                            ( Select
                                                { distinct = False
                                                , columns = ResultColumn'Expression
                                                    ( Aliased
                                                        { value = Expression'Subquery
                                                            ( SelectStatement
                                                                { commonTableExpressions = Nothing
                                                                , select = CompoundSelect
                                                                    ( SelectCore'Select
                                                                        ( Select
                                                                            { distinct = False
                                                                            , columns = ResultColumn'Expression
                                                                                ( Aliased
                                                                                    { value = Expression'Plus
                                                                                        ( Expression'Plus
                                                                                            ( Expression'FunctionCall
                                                                                                ( FunctionCallExpression
                                                                                                    { call = FunctionCall
                                                                                                        { name = Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "max"
                                                                                                            }
                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                            [ Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "c7"
                                                                                                                    }
                                                                                                                )
                                                                                                            ]
                                                                                                        }
                                                                                                    , filter = Nothing
                                                                                                    , over = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                            ( Expression'FunctionCall
                                                                                                ( FunctionCallExpression
                                                                                                    { call = FunctionCall
                                                                                                        { name = Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "max"
                                                                                                            }
                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                            [ Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "c8"
                                                                                                                    }
                                                                                                                )
                                                                                                            ]
                                                                                                        }
                                                                                                    , filter = Nothing
                                                                                                    , over = Nothing
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        )
                                                                                        ( Expression'FunctionCall
                                                                                            ( FunctionCallExpression
                                                                                                { call = FunctionCall
                                                                                                    { name = Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "max"
                                                                                                        }
                                                                                                    , arguments = FunctionArguments'Arguments
                                                                                                        [ Expression'Column
                                                                                                            ( Namespaced
                                                                                                                { namespace = Nothing
                                                                                                                , value = "c9"
                                                                                                                }
                                                                                                            )
                                                                                                        ]
                                                                                                    }
                                                                                                , filter = Nothing
                                                                                                , over = Nothing
                                                                                                }
                                                                                            )
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
                                                                                                , value = "t9"
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
                                                                    , value = "t8"
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
                                        , value = "t7"
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
    )