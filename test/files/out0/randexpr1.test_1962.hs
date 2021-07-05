Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'Multiply
                                ( Expression'Column
                                    ( Namespaced
                                        { namespace = Nothing
                                        , value = "e"
                                        }
                                    )
                                )
                                ( Expression'Subquery
                                    ( SelectStatement
                                        { commonTableExpressions = Nothing
                                        , select = CompoundSelect
                                            ( SelectCore'Select
                                                ( Select
                                                    { distinct = False
                                                    , columns = ResultColumn'Expression
                                                        ( Aliased
                                                            { value = Expression'Negate
                                                                ( Expression'FunctionCall
                                                                    ( FunctionCallExpression
                                                                        { call = FunctionCall
                                                                            { name = Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "max"
                                                                                }
                                                                            , arguments = FunctionArguments'Arguments
                                                                                [ Expression'Multiply
                                                                                    ( Expression'Minus
                                                                                        ( Expression'Case
                                                                                            ( CaseExpression
                                                                                                { base = Nothing
                                                                                                , cases =
                                                                                                    ( Expression'NotEquals
                                                                                                        ( Expression'BitwiseOr
                                                                                                            ( Expression'Negate
                                                                                                                ( Expression'Case
                                                                                                                    ( CaseExpression
                                                                                                                        { base = Nothing
                                                                                                                        , cases =
                                                                                                                            ( Expression'GreaterThan
                                                                                                                                ( Expression'Multiply
                                                                                                                                    ( Expression'Divide
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
                                                                                                                                                                , value = "f"
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
                                                                                                                                                        [ Expression'LiteralValue
                                                                                                                                                            ( Number "17" )
                                                                                                                                                        ]
                                                                                                                                                    }
                                                                                                                                                , filter = Nothing
                                                                                                                                                , over = Nothing
                                                                                                                                                }
                                                                                                                                            )
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                        ( Number "13" )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                                ( Expression'Negate
                                                                                                                                    ( Expression'Column
                                                                                                                                        ( Namespaced
                                                                                                                                            { namespace = Nothing
                                                                                                                                            , value = "a"
                                                                                                                                            }
                                                                                                                                        )
                                                                                                                                    )
                                                                                                                                )
                                                                                                                            , Expression'LiteralValue
                                                                                                                                ( Number "17" )
                                                                                                                            ) :| []
                                                                                                                        , else_ = Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "f"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Minus
                                                                                                                ( Expression'Negate
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
                                                                                                                ( Expression'Multiply
                                                                                                                    ( Expression'Column
                                                                                                                        ( Namespaced
                                                                                                                            { namespace = Nothing
                                                                                                                            , value = "e"
                                                                                                                            }
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'LiteralValue
                                                                                                                        ( Number "11" )
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                        )
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "11" )
                                                                                                        )
                                                                                                    , Expression'Column
                                                                                                        ( Namespaced
                                                                                                            { namespace = Nothing
                                                                                                            , value = "c"
                                                                                                            }
                                                                                                        )
                                                                                                    ) :|
                                                                                                    [
                                                                                                        ( Expression'Or
                                                                                                            ( Expression'Exists
                                                                                                                ( SelectStatement
                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                    , select = CompoundSelect
                                                                                                                        ( SelectCore'Select
                                                                                                                            ( Select
                                                                                                                                { distinct = False
                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                    ( Aliased
                                                                                                                                        { value = Expression'LiteralValue
                                                                                                                                            ( Number "1" )
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
                                                                                                                                    ( Expression'Not
                                                                                                                                        ( Expression'Exists
                                                                                                                                            ( SelectStatement
                                                                                                                                                { commonTableExpressions = Nothing
                                                                                                                                                , select = CompoundSelect
                                                                                                                                                    ( SelectCore'Select
                                                                                                                                                        ( Select
                                                                                                                                                            { distinct = False
                                                                                                                                                            , columns = ResultColumn'Expression
                                                                                                                                                                ( Aliased
                                                                                                                                                                    { value = Expression'LiteralValue
                                                                                                                                                                        ( Number "1" )
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
                                                                                                                                                                ( Expression'And
                                                                                                                                                                    ( Expression'Not
                                                                                                                                                                        ( Expression'InValues
                                                                                                                                                                            ( InValuesExpression
                                                                                                                                                                                { expression = Expression'Column
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
                                                                                                                                                                                , values =
                                                                                                                                                                                    [ Expression'Column
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
                                                                                                                                                                                        ( Number "19" )
                                                                                                                                                                                    , Expression'Column
                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                            }
                                                                                                                                                                                        )
                                                                                                                                                                                    ]
                                                                                                                                                                                }
                                                                                                                                                                            )
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                    ( Expression'NotEquals
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
                                                                                                                                                                        ( Expression'LiteralValue
                                                                                                                                                                            ( Number "11" )
                                                                                                                                                                        )
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
                                                                                                            )
                                                                                                            ( Expression'LessThanOrEquals
                                                                                                                ( Expression'LiteralValue
                                                                                                                    ( Number "13" )
                                                                                                                )
                                                                                                                ( Expression'Negate
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
                                                                                                            )
                                                                                                        , Expression'LiteralValue
                                                                                                            ( Number "11" )
                                                                                                        )
                                                                                                    ]
                                                                                                , else_ = Expression'Column
                                                                                                    ( Namespaced
                                                                                                        { namespace = Nothing
                                                                                                        , value = "d"
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
                                                                                                , value = "a"
                                                                                                }
                                                                                            )
                                                                                        )
                                                                                    )
                                                                                    ( Expression'Column
                                                                                        ( Namespaced
                                                                                            { namespace = Nothing
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
                        ( Expression'Not
                            ( Expression'Exists
                                ( SelectStatement
                                    { commonTableExpressions = Nothing
                                    , select = CompoundSelect
                                        ( SelectCore'Select
                                            ( Select
                                                { distinct = False
                                                , columns = ResultColumn'Expression
                                                    ( Aliased
                                                        { value = Expression'LiteralValue
                                                            ( Number "1" )
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
                                                                { namespace = Nothing
                                                                , value = "c"
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
                                                , groupBy = Nothing
                                                , window = Nothing
                                                }
                                            )
                                        )
                                    , orderBy = Nothing
                                    , limit = Nothing
                                    }
                                )
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