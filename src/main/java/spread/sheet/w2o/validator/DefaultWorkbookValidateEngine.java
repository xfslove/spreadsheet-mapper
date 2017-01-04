package spread.sheet.w2o.validator;

import org.apache.commons.collections.CollectionUtils;
import spread.sheet.model.core.Workbook;
import spread.sheet.model.message.ErrorMessage;
import spread.sheet.model.message.ErrorMessageBean;
import spread.sheet.model.message.MessageWriteStrategies;
import spread.sheet.w2o.validator.workbook.WorkbookValidator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookValidateEngine implements WorkbookValidateEngine {

  private Workbook workbook;

  private List<WorkbookValidator> workbookValidators = new ArrayList<>();

  private List<SheetValidateEngine> sheetValidateEngines = new ArrayList<>();

  private List<ErrorMessage> errorMessages = new ArrayList<>();

  // write workbook error message on sheet 1.
  private static final int WORKBOOK_ERROR_MESSAGE_ON_SHEET = 1;

  @Override
  public WorkbookValidateEngine workbookValidator(WorkbookValidator... workbookValidators) {
    if (workbookValidators == null) {
      return this;
    }
    Collections.addAll(this.workbookValidators, workbookValidators);
    return this;
  }

  @Override
  public WorkbookValidateEngine sheetValidateEngine(SheetValidateEngine... sheetValidateEngines) {
    if (sheetValidateEngines == null) {
      return this;
    }
    Collections.addAll(this.sheetValidateEngines, sheetValidateEngines);
    return this;
  }

  public WorkbookValidateEngine workbook(Workbook workbook) {
    this.workbook = workbook;
    return this;
  }

  @Override
  public boolean valid() {
    if (workbook == null) {
      throw new WorkbookValidateException("set workbook first");
    }
    validWorkbook(workbook);

    if (CollectionUtils.isNotEmpty(errorMessages)) {
      return false;
    }

    boolean sheetValidResult = true;

    for (SheetValidateEngine sheetValidateEngine : sheetValidateEngines) {

      if (!sheetValidateEngine.valid()) {
        errorMessages.addAll(sheetValidateEngine.getErrorMessages());
        sheetValidResult = false;
      }
    }

    return sheetValidResult;
  }

  @Override
  public List<ErrorMessage> getErrorMessages() {
    return errorMessages;
  }

  /*==============
    workbook valid
   ===============*/
  private void validWorkbook(Workbook workbook) {

    for (WorkbookValidator validator : workbookValidators) {
      if (!validator.valid(workbook)) {
        errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, validator.getErrorMessage(), WORKBOOK_ERROR_MESSAGE_ON_SHEET));
      }
    }

  }
}
