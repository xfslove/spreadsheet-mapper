package me.excel.tools.validator.row;


import me.excel.tools.model.excel.Row;

import java.util.function.Function;

/**
 * customer row validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class CommonRowValidator extends RowValidatorAdapter {

  private Function<Row, Boolean> validateResultGetter;

  public CommonRowValidator(Function<Row, Boolean> validateResultGetter, String errorMessage, String[] messageOnFields) {
    super(errorMessage, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  public CommonRowValidator(Function<Row, Boolean> validateResultGetter, String[] messageOnFields) {
    super(null, messageOnFields);
    this.validateResultGetter = validateResultGetter;
  }

  @Override
  protected boolean customValidate(Row row) {
    if (validateResultGetter == null) {
      return true;
    }
    return validateResultGetter.apply(row);
  }
}
