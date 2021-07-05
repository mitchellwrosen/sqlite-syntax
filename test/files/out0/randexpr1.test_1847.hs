Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Divide
                                ( Expression'FunctionCall
                                    ( FunctionCallExpression
                                        { call = FunctionCall
                                            { name = Namespaced
                                                { namespace = Nothing
                                                , value = "abs"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'BitwiseAnd
                                                    ( Expression'LiteralValue
                                                        ( Number "11" )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Just
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "t1"
                                                                    }
                                                                )
                                                            , value = "e"
                                                            }
                                                        )
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
                                                , value = "abs"
                                                }
                                            , arguments = FunctionArguments'Arguments
                                                [ Expression'Case
                                                    ( CaseExpression
                                                        { base = Just
                                                            ( Expression'LiteralValue
                                                                ( Number "13" )
                                                            )
                                                        , cases =
                                                            ( Expression'Plus
                                                                ( Expression'Multiply
                                                                    ( Expression'LiteralValue
                                                                        ( Number "17" )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "t1"
                                                                                    }
                                                                                )
                                                                            , value = "a"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                                ( Expression'Multiply
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "d"
                                                                            }
                                                                        )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "d"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                            , Expression'Plus
                                                                ( Expression'Plus
                                                                    ( Expression'Minus
                                                                        ( Expression'Multiply
                                                                            ( Expression'BitwiseNegate
                                                                                ( Expression'Negate
                                                                                    ( Expression'LiteralValue
                                                                                        ( Number "17" )
                                                                                    )
                                                                                )
                                                                            )
                                                                            ( Expression'Negate
                                                                                ( Expression'Negate
                                                                                    ( Expression'Case
                                                                                        ( CaseExpression
                                                                                            { base = Just
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "f"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            , cases =
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
                                                                                                , Expression'Minus
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "a"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                    ( Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "b"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                ) :| []
                                                                                            , else_ = Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Just
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "t1"
                                                                                                            }
                                                                                                        )
                                                                                                    , value = "d"
                                                                                                    }
                                                                                                )
                                                                                            }
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                        ( Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "f"
                                                                                }
                                                                            )
                                                                        )
                                                                    )
                                                                    ( Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Just
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "t1"
                                                                                    }
                                                                                )
                                                                            , value = "f"
                                                                            }
                                                                        )
                                                                    )
                                                                )
                                                                ( Expression'LiteralValue
                                                                    ( Number "17" )
                                                                )
                                                            ) :| []
                                                        , else_ = Expression'Column
                                                            ( Namespaced
                                                                { namespace = Just
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "t1"
                                                                        }
                                                                    )
                                                                , value = "a"
                                                                }
                                                            )
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
                                        , value = "t1"
                                        }
                                    , alias = Nothing
                                    }
                                , indexedBy = Nothing
                                }
                            )
                        )
                    , where_ = Just
                        ( Expression'NotEquals
                            ( Expression'Column
                                ( Namespaced
                                    { namespace = Just
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "t1"
                                            }
                                        )
                                    , value = "f"
                                    }
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "17" )
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