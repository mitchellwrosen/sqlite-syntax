Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'BitwiseAnd
                                ( Expression'Minus
                                    ( Expression'Minus
                                        ( Expression'Multiply
                                            ( Expression'Negate
                                                ( Expression'BitwiseNegate
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "d"
                                                            }
                                                        )
                                                    )
                                                )
                                            )
                                            ( Expression'LiteralValue
                                                ( Number "19" )
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
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Nothing
                                            , value = "c"
                                            }
                                        )
                                    )
                                )
                                ( Expression'Plus
                                    ( Expression'Plus
                                        ( Expression'Column
                                            ( Namespaced
                                                { namespace = Just
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "t1"
                                                        }
                                                    )
                                                , value = "c"
                                                }
                                            )
                                        )
                                        ( Expression'Divide
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
                                                                        ( Expression'BitwiseAnd
                                                                            ( Expression'Negate
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "19" )
                                                                                )
                                                                            )
                                                                            ( Expression'BitwiseNegate
                                                                                ( Expression'Case
                                                                                    ( CaseExpression
                                                                                        { base = Just
                                                                                            ( Expression'Column
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
                                                                                            )
                                                                                        , cases =
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "c"
                                                                                                    }
                                                                                                )
                                                                                            , Expression'Column
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
                                                                                            ) :| []
                                                                                        , else_ = Expression'Column
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        }
                                                                                    )
                                                                                )
                                                                            )
                                                                        )
                                                                    , cases =
                                                                        ( Expression'Column
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
                                                                        , Expression'LiteralValue
                                                                            ( Number "17" )
                                                                        ) :| []
                                                                    , else_ = Expression'Case
                                                                        ( CaseExpression
                                                                            { base = Just
                                                                                ( Expression'Case
                                                                                    ( CaseExpression
                                                                                        { base = Nothing
                                                                                        , cases =
                                                                                            ( Expression'GreaterThan
                                                                                                ( Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( Expression'Minus
                                                                                                    ( Expression'Case
                                                                                                        ( CaseExpression
                                                                                                            { base = Just
                                                                                                                ( Expression'Plus
                                                                                                                    ( Expression'Plus
                                                                                                                        ( Expression'Case
                                                                                                                            ( CaseExpression
                                                                                                                                { base = Nothing
                                                                                                                                , cases =
                                                                                                                                    ( Expression'Or
                                                                                                                                        ( Expression'Equals
                                                                                                                                            ( Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Just
                                                                                                                                                        ( Namespaced
                                                                                                                                                            { namespace = Nothing
                                                                                                                                                            , value = "t1"
                                                                                                                                                            }
                                                                                                                                                        )
                                                                                                                                                    , value = "c"
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
                                                                                                                                        ( Expression'Equals
                                                                                                                                            ( Expression'Negate
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
                                                                                                                                            ( Expression'LiteralValue
                                                                                                                                                ( Number "17" )
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    , Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    ) :|
                                                                                                                                    [
                                                                                                                                        ( Expression'NotEquals
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
                                                                                                                                            ( Expression'Column
                                                                                                                                                ( Namespaced
                                                                                                                                                    { namespace = Nothing
                                                                                                                                                    , value = "d"
                                                                                                                                                    }
                                                                                                                                                )
                                                                                                                                            )
                                                                                                                                        , Expression'Column
                                                                                                                                            ( Namespaced
                                                                                                                                                { namespace = Just
                                                                                                                                                    ( Namespaced
                                                                                                                                                        { namespace = Nothing
                                                                                                                                                        , value = "t1"
                                                                                                                                                        }
                                                                                                                                                    )
                                                                                                                                                , value = "c"
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                , else_ = Expression'Column
                                                                                                                                    ( Namespaced
                                                                                                                                        { namespace = Nothing
                                                                                                                                        , value = "a"
                                                                                                                                        }
                                                                                                                                    )
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "11" )
                                                                                                                    )
                                                                                                                )
                                                                                                            , cases =
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "19" )
                                                                                                                , Expression'Column
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
                                                                                                                ) :| []
                                                                                                            , else_ = Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            }
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
                                                                                                            , value = "e"
                                                                                                            }
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                            , Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "f"
                                                                                                    }
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
                                                                                                , value = "c"
                                                                                                }
                                                                                            )
                                                                                        }
                                                                                    )
                                                                                )
                                                                            , cases =
                                                                                ( Expression'LiteralValue
                                                                                    ( Number "17" )
                                                                                , Expression'Column
                                                                                    ( Namespaced
                                                                                        { namespace = Just
                                                                                            ( Namespaced
                                                                                                { namespace = Nothing
                                                                                                , value = "t1"
                                                                                                }
                                                                                            )
                                                                                        , value = "b"
                                                                                        }
                                                                                    )
                                                                                ) :| []
                                                                            , else_ = Expression'Column
                                                                                ( Namespaced
                                                                                    { namespace = Nothing
                                                                                    , value = "a"
                                                                                    }
                                                                                )
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
                                            ( Expression'FunctionCall
                                                ( FunctionCallExpression
                                                    { call = FunctionCall
                                                        { name = Namespaced
                                                            { namespace = Nothing
                                                            , value = "abs"
                                                            }
                                                        , arguments = FunctionArguments'Arguments
                                                            [ Expression'Column
                                                                ( Namespaced
                                                                    { namespace = Nothing
                                                                    , value = "d"
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
                                    )
                                    ( Expression'Column
                                        ( Namespaced
                                            { namespace = Just
                                                ( Namespaced
                                                    { namespace = Nothing
                                                    , value = "t1"
                                                    }
                                                )
                                            , value = "b"
                                            }
                                        )
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
                            ( Expression'Plus
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
                                    ( Number "13" )
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "19" )
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