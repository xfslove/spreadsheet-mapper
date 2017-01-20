package spreadsheet.mapper.w2o.validation.engine;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.core.Row;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.msg.Message;
import spreadsheet.mapper.model.msg.MessageBean;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.validation.WorkbookValidateException;
import spreadsheet.mapper.w2o.validation.validator.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidator;

import java.util.*;

/**
 * use simply dfs to do dependency validate, depends on dependencies no cycle {@link DependencyCycleCheckEngine}
 * <p>
 * Created by hanwen on 2017/1/6.
 */
public class DependencyValidateEngine {

  private static final Logger LOGGER = LoggerFactory.getLogger(DependencyValidateEngine.class);

  private Map<String, List<DependencyValidator>> validatorMap = new LinkedHashMap<>();

  private Map<String, Set<String>> vGraph = new LinkedHashMap<>();
  private Map<String, Boolean> visited = new HashMap<>();

  // is skip valid rest validator in one search tree
  private boolean skip;

  private List<Message> errorMessages = new ArrayList<>();
  private boolean validResult = true;

  public DependencyValidateEngine(Map<String, List<DependencyValidator>> validatorMap) {
    this.vGraph = DependencyEngineHelper.buildVGraph(validatorMap);
    for (String v : vGraph.keySet()) {
      visited.put(v, false);
    }
    this.validatorMap = validatorMap;
  }

  public boolean valid(Row row, SheetMeta sheetMeta) {

    for (String v : vGraph.keySet()) {
      skip = false;
      if (!visited.get(v)) {
        dfs(row, sheetMeta, v);
      }
    }

    return validResult;
  }

  public List<Message> getErrorMessages() {
    return errorMessages;
  }

  private void dfs(Row row, SheetMeta sheetMeta, String v) {

    for (String w : vGraph.get(v)) {
      if (!visited.get(w)) {
        dfs(row, sheetMeta, w);
      }
    }

    visited.put(v, true);

    if (skip) {
      LOGGER.debug("skip valid at group:[" + v + "]");
      return;
    }

    List<DependencyValidator> validators = validatorMap.get(v);

    LOGGER.debug("do valid at group:[" + v + "] and validator numbers of this group is:[" + validators.size() + "]");

    for (DependencyValidator validator : validators) {
      boolean vResult = doRowCellsValid(row, sheetMeta, validator);
      // if one of the group valid failure skip rest validators
      if (!vResult) {
        skip = true;
        break;
      }
    }
  }

  private boolean doRowCellsValid(Row row, SheetMeta sheetMeta, DependencyValidator dependencyValidator) {
    if (dependencyValidator instanceof RowValidator) {

      RowValidator rowValidator = (RowValidator) dependencyValidator;
      boolean result = rowValidator.valid(row, sheetMeta);

      if (!result) {

        validResult = false;
        String errorMessage = rowValidator.getErrorMessage();
        Set<String> messageOnFields = rowValidator.getMessageOnFields();

        if (StringUtils.isNotBlank(errorMessage) && CollectionUtils.isNotEmpty(messageOnFields)) {
          for (String messageOnField : messageOnFields) {
            FieldMeta fieldMeta = sheetMeta.getFieldMeta(messageOnField);

            errorMessages.add(new MessageBean(MessageWriteStrategies.COMMENT, errorMessage, row.getSheet().getIndex(), row.getIndex(), fieldMeta.getColumnIndex()));
          }
        }

      }

      return result;
    } else {

      CellValidator cellValidator = (CellValidator) dependencyValidator;
      String matchField = cellValidator.getMatchField();
      if (StringUtils.isBlank(matchField)) {
        throw new WorkbookValidateException("cell validator[" + cellValidator.getClass().getName() + "] match field can not be null");
      }

      FieldMeta fieldMeta = sheetMeta.getFieldMeta(matchField);

      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      boolean result = cellValidator.valid(cell, fieldMeta);

      if (!result) {

        validResult = false;
        String errorMessage = cellValidator.getErrorMessage();
        String messageOnField = cellValidator.getMessageOnField();

        if (StringUtils.isNotBlank(errorMessage) && StringUtils.isNotBlank(messageOnField)) {
          FieldMeta messageOnFieldMeta = sheetMeta.getFieldMeta(messageOnField);

          errorMessages.add(new MessageBean(MessageWriteStrategies.COMMENT, errorMessage, row.getSheet().getIndex(), row.getIndex(), messageOnFieldMeta.getColumnIndex()));
        }

      }

      return result;
    }
  }
}
