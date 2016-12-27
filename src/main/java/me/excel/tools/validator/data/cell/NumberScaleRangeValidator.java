package me.excel.tools.validator.data.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.math.NumberUtils;

/**
 * number scale range validator
 * <p>
 * Created by hanwen on 2016/12/1.
 */
public class NumberScaleRangeValidator extends CellValidatorAdapter {

  private int gte;

  private int lte;

  public NumberScaleRangeValidator(String matchField, int gte, int lte) {
    super(matchField, "小数位数范围[" + gte + ", " + lte + "]");
    this.gte = gte;
    this.lte = lte;
  }

  public NumberScaleRangeValidator(String matchField, int gte, int lte, String errorMessage) {
    super(matchField, errorMessage);
    this.gte = gte;
    this.lte = lte;
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {

    String value = excelCell.getValue();

    if (!NumberUtils.isNumber(value)) {
      return false;
    }

    String[] numberPlace = value.split("\\.");

    int scale = 0;
    if (numberPlace.length != 1) {
      scale = numberPlace[1].length();
    }

    return gte <= scale && scale <= lte;
  }
}
