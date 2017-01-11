package spreadsheet.mapper.w2o.validator;

import org.apache.commons.collections.CollectionUtils;
import spreadsheet.mapper.model.core.Workbook;
import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.model.msg.ErrorMessageBean;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.validator.workbook.WorkbookValidator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by hanwen on 2017/1/4.
 */
public class DefaultWorkbookValidateHelper implements WorkbookValidateHelper {

  private Workbook workbook;

  private List<WorkbookValidator> workbookValidators = new ArrayList<>();

  private List<SheetValidateHelper> sheetValidateHelpers = new ArrayList<>();

  private List<ErrorMessage> errorMessages = new ArrayList<>();

  // write workbook error message on sheet 1.
  private static final int WORKBOOK_ERROR_MESSAGE_ON_SHEET = 1;

  @Override
  public WorkbookValidateHelper workbookValidator(WorkbookValidator... workbookValidators) {
    if (workbookValidators == null) {
      return this;
    }
    Collections.addAll(this.workbookValidators, workbookValidators);
    return this;
  }

  @Override
  public WorkbookValidateHelper sheetValidateEngine(SheetValidateHelper... sheetValidateHelpers) {
    if (sheetValidateHelpers == null) {
      return this;
    }
    Collections.addAll(this.sheetValidateHelpers, sheetValidateHelpers);
    return this;
  }

  public WorkbookValidateHelper workbook(Workbook workbook) {
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

    for (SheetValidateHelper sheetValidateHelper : sheetValidateHelpers) {

      if (!sheetValidateHelper.valid()) {
        errorMessages.addAll(sheetValidateHelper.getErrorMessages());
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
