package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

import static me.excel.tools.utils.BooleanTranslator.isValidTrue;
import static me.excel.tools.utils.BooleanTranslator.isValidFalse;

/**
 * Created by hanwen on 15-12-18.
 */
public class BooleanValidator extends AbstractFieldValidator {

  public BooleanValidator(String matchField) {
    super(matchField,
        "数据不正确, 格式应为: \'是\':true,t,是,yes,y,1; \'否\':false,f,否,no,n,0",
        "\'是\':true,t,是,yes,y,1; \'否\':false,f,否,no,n,0");
  }

  public BooleanValidator(String matchField, String errorMessage, String prompt) {
    super(matchField, errorMessage, prompt);
  }

  @Override
  public boolean validate(ExcelCell excelCell) {
    return isValidTrue(excelCell.getValue()) && isValidFalse(excelCell.getValue());
  }


}
