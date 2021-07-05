Statement'Select
    ( SelectStatement
        { commonTableExpressions = Nothing
        , select = CompoundSelect
            ( SelectCore'Select
                ( Select
                    { distinct = False
                    , columns = ResultColumn'Expression
                        ( Aliased
                            { value = Expression'FunctionCall
                                ( FunctionCallExpression
                                    { call = FunctionCall
                                        { name = Namespaced
                                            { namespace = Nothing
                                            , value = "coalesce"
                                            }
                                        , arguments = FunctionArguments'Arguments
                                            [ Expression'Subquery
                                                ( SelectStatement
                                                    { commonTableExpressions = Nothing
                                                    , select = CompoundSelect
                                                        ( SelectCore'Select
                                                            ( Select
                                                                { distinct = False
                                                                , columns = ResultColumn'Expression
                                                                    ( Aliased
                                                                        { value = Expression'FunctionCall
                                                                            ( FunctionCallExpression
                                                                                { call = FunctionCall
                                                                                    { name = Namespaced
                                                                                        { namespace = Nothing
                                                                                        , value = "max"
                                                                                        }
                                                                                    , arguments = FunctionArguments'Arguments
                                                                                        [ Expression'Plus
                                                                                            ( Expression'Minus
                                                                                                ( Expression'Minus
                                                                                                    ( Expression'Minus
                                                                                                        ( Expression'LiteralValue
                                                                                                            ( Number "11" )
                                                                                                        )
                                                                                                        ( Expression'Negate
                                                                                                            ( Expression'LiteralValue
                                                                                                                ( Number "19" )
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
                                                                                                        , value = "b"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                            ( Expression'Column
                                                                                                ( Namespaced
                                                                                                    { namespace = Nothing
                                                                                                    , value = "a"
                                                                                                    }
                                                                                                )
                                                                                            )
                                                                                        ]
                                                                                    }
                                                                                , filter = Nothing
                                                                                , over = Nothing
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
                                                                                    , value = "t1"
                                                                                    }
                                                                                , alias = Nothing
                                                                                }
                                                                            , indexedBy = Nothing
                                                                            }
                                                                        )
                                                                    )
                                                                , where_ = Just
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
                                                                                                ( Expression'InValues
                                                                                                    ( InValuesExpression
                                                                                                        { expression = Expression'BitwiseOr
                                                                                                            ( Expression'Plus
                                                                                                                ( Expression'Minus
                                                                                                                    ( Expression'Minus
                                                                                                                        ( Expression'Minus
                                                                                                                            ( Expression'Minus
                                                                                                                                ( Expression'LiteralValue
                                                                                                                                    ( Number "11" )
                                                                                                                                )
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
                                                                                                                            ( Expression'Multiply
                                                                                                                                ( Expression'Multiply
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
                                                                                                                                ( Expression'BitwiseNegate
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
                                                                                                                ( Expression'FunctionCall
                                                                                                                    ( FunctionCallExpression
                                                                                                                        { call = FunctionCall
                                                                                                                            { name = Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "coalesce"
                                                                                                                                }
                                                                                                                            , arguments = FunctionArguments'Arguments
                                                                                                                                [ Expression'Subquery
                                                                                                                                    ( SelectStatement
                                                                                                                                        { commonTableExpressions = Nothing
                                                                                                                                        , select = CompoundSelect
                                                                                                                                            ( SelectCore'Select
                                                                                                                                                ( Select
                                                                                                                                                    { distinct = False
                                                                                                                                                    , columns = ResultColumn'Expression
                                                                                                                                                        ( Aliased
                                                                                                                                                            { value = Expression'FunctionCall
                                                                                                                                                                ( FunctionCallExpression
                                                                                                                                                                    { call = FunctionCall
                                                                                                                                                                        { name = Namespaced
                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                            , value = "coalesce"
                                                                                                                                                                            }
                                                                                                                                                                        , arguments = FunctionArguments'Arguments
                                                                                                                                                                            [ Expression'Subquery
                                                                                                                                                                                ( SelectStatement
                                                                                                                                                                                    { commonTableExpressions = Nothing
                                                                                                                                                                                    , select = CompoundSelect
                                                                                                                                                                                        ( SelectCore'Select
                                                                                                                                                                                            ( Select
                                                                                                                                                                                                { distinct = False
                                                                                                                                                                                                , columns = ResultColumn'Expression
                                                                                                                                                                                                    ( Aliased
                                                                                                                                                                                                        { value = Expression'Column
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
                                                                                                                                                                                                    ( Expression'Equals
                                                                                                                                                                                                        ( Expression'Case
                                                                                                                                                                                                            ( CaseExpression
                                                                                                                                                                                                                { base = Just
                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "c"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    )
                                                                                                                                                                                                                , cases =
                                                                                                                                                                                                                    ( Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "d"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    , Expression'Column
                                                                                                                                                                                                                        ( Namespaced
                                                                                                                                                                                                                            { namespace = Nothing
                                                                                                                                                                                                                            , value = "e"
                                                                                                                                                                                                                            }
                                                                                                                                                                                                                        )
                                                                                                                                                                                                                    ) :| []
                                                                                                                                                                                                                , else_ = Expression'LiteralValue
                                                                                                                                                                                                                    ( Number "11" )
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
                                                                                                                                                                                                                , value = "f"
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
                                                                                                                                                                            , Expression'Column
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
                                                                                                                                                                            ]
                                                                                                                                                                        }
                                                                                                                                                                    , filter = Nothing
                                                                                                                                                                    , over = Nothing
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
                                                                                                                                                                        , value = "t1"
                                                                                                                                                                        }
                                                                                                                                                                    , alias = Nothing
                                                                                                                                                                    }
                                                                                                                                                                , indexedBy = Nothing
                                                                                                                                                                }
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    , where_ = Just
                                                                                                                                                        ( Expression'GreaterThan
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
                                                                                                                                                            ( Expression'Column
                                                                                                                                                                ( Namespaced
                                                                                                                                                                    { namespace = Nothing
                                                                                                                                                                    , value = "b"
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
                                                                                                                                , Expression'Negate
                                                                                                                                    ( Expression'LiteralValue
                                                                                                                                        ( Number "11" )
                                                                                                                                    )
                                                                                                                                ]
                                                                                                                            }
                                                                                                                        , filter = Nothing
                                                                                                                        , over = Nothing
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Plus
                                                                                                                ( Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "f"
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
                                                                                                                        , value = "f"
                                                                                                                        }
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                        , values =
                                                                                                            [ Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "c"
                                                                                                                    }
                                                                                                                )
                                                                                                            , Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "b"
                                                                                                                    }
                                                                                                                )
                                                                                                            , Expression'LiteralValue
                                                                                                                ( Number "13" )
                                                                                                            ]
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
                                                                , groupBy = Nothing
                                                                , window = Nothing
                                                                }
                                                            )
                                                        )
                                                    , orderBy = Nothing
                                                    , limit = Nothing
                                                    }
                                                )
                                            , Expression'Column
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
                        ( Expression'Equals
                            ( Expression'Minus
                                ( Expression'Case
                                    ( CaseExpression
                                        { base = Nothing
                                        , cases =
                                            ( Expression'GreaterThanOrEquals
                                                ( Expression'Plus
                                                    ( Expression'LiteralValue
                                                        ( Number "19" )
                                                    )
                                                    ( Expression'Column
                                                        ( Namespaced
                                                            { namespace = Nothing
                                                            , value = "c"
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
                                                        , value = "a"
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
                                            ) :|
                                            [
                                                ( Expression'Not
                                                    ( Expression'NotEquals
                                                        ( Expression'Case
                                                            ( CaseExpression
                                                                { base = Nothing
                                                                , cases =
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
                                                                                                    ( Expression'BitwiseOr
                                                                                                        ( Expression'BitwiseOr
                                                                                                            ( Expression'Column
                                                                                                                ( Namespaced
                                                                                                                    { namespace = Nothing
                                                                                                                    , value = "f"
                                                                                                                    }
                                                                                                                )
                                                                                                            )
                                                                                                            ( Expression'Minus
                                                                                                                ( Expression'Plus
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'Multiply
                                                                                                                            ( Expression'Column
                                                                                                                                ( Namespaced
                                                                                                                                    { namespace = Nothing
                                                                                                                                    , value = "b"
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
                                                                                                                        )
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        )
                                                                                                                    )
                                                                                                                    ( Expression'Multiply
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "19" )
                                                                                                                        )
                                                                                                                        ( Expression'LiteralValue
                                                                                                                            ( Number "13" )
                                                                                                                        )
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
                                                                                                        )
                                                                                                        ( Expression'Case
                                                                                                            ( CaseExpression
                                                                                                                { base = Nothing
                                                                                                                , cases =
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
                                                                                                                                        , value = "e"
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
                                                                                                                                            , value = "f"
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
                                                                                                                                            , value = "c"
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
                                                                                                                            )
                                                                                                                        )
                                                                                                                    , Expression'LiteralValue
                                                                                                                        ( Number "11" )
                                                                                                                    ) :|
                                                                                                                    [
                                                                                                                        ( Expression'GreaterThan
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
                                                                                                                                    , value = "c"
                                                                                                                                    }
                                                                                                                                )
                                                                                                                            )
                                                                                                                        , Expression'Column
                                                                                                                            ( Namespaced
                                                                                                                                { namespace = Nothing
                                                                                                                                , value = "a"
                                                                                                                                }
                                                                                                                            )
                                                                                                                        )
                                                                                                                    ]
                                                                                                                , else_ = Expression'Column
                                                                                                                    ( Namespaced
                                                                                                                        { namespace = Nothing
                                                                                                                        , value = "e"
                                                                                                                        }
                                                                                                                    )
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
                                                                    , Expression'Column
                                                                        ( Namespaced
                                                                            { namespace = Nothing
                                                                            , value = "b"
                                                                            }
                                                                        )
                                                                    ) :|
                                                                    [
                                                                        ( Expression'Equals
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
                                                                                    , value = "e"
                                                                                    }
                                                                                )
                                                                            )
                                                                        , Expression'Column
                                                                            ( Namespaced
                                                                                { namespace = Nothing
                                                                                , value = "b"
                                                                                }
                                                                            )
                                                                        )
                                                                    ]
                                                                , else_ = Expression'Column
                                                                    ( Namespaced
                                                                        { namespace = Nothing
                                                                        , value = "e"
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
                                                                , value = "b"
                                                                }
                                                            )
                                                        )
                                                    )
                                                , Expression'Column
                                                    ( Namespaced
                                                        { namespace = Nothing
                                                        , value = "a"
                                                        }
                                                    )
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
                                        , value = "b"
                                        }
                                    )
                                )
                            )
                            ( Expression'LiteralValue
                                ( Number "13" )
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