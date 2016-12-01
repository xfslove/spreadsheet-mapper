package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.validator.SkipValidateException;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * Created by hanwen on 2016/12/1.
 */
public class NumberScaleValidator extends AbstractCellValidator {

  private int gte;

  private int lte;

  public NumberScaleValidator(String matchField, int gte, int lte) {
    super(matchField, "小数位数范围[" + gte + ", " + lte + "]", "小数位数范围[" + gte + ", " + lte + "]");
    this.gte = gte;
    this.lte = lte;
  }

  public NumberScaleValidator(String matchField, String errorMessage, String prompt) {
    super(matchField, errorMessage, prompt);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) throws SkipValidateException {

    String value = excelCell.getValue();

    if (!NumberUtils.isNumber(value)) {
      return false;
    }

    String[] numberPlace = value.split("\\.");

    if (numberPlace.length == 1) {
      return false;
    }

    int scale = numberPlace[1].length();

    return gte <= scale && scale <= lte;
  }
}
