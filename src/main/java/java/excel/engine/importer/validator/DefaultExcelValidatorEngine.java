package java.excel.engine.importer.validator;

import java.excel.engine.exception.ExcelReadException;
import java.excel.engine.exception.ExcelValidateException;
import java.excel.engine.model.excel.Row;
import java.excel.engine.model.excel.Sheet;
import java.excel.engine.model.excel.Workbook;
import java.excel.engine.model.message.DataValidateMessage;
import java.excel.engine.importer.validator.cell.CellValidator;
import java.excel.engine.importer.validator.row.RowValidator;
import java.excel.engine.importer.validator.sheet.SheetValidator;
import java.excel.engine.importer.validator.workbook.WorkbookValidator;
import org.apache.commons.collections.CollectionUtils;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultExcelValidatorEngine implements ExcelValidatorEngine {

  /*===============
    validators
   ================*/
  private List<WorkbookValidator> workbookValidators = new ArrayList<>();
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private Map<Integer, Map<String, List<RowValidator>>> key2rowValidators = new HashMap<>();
  private Map<Integer, Map<String, List<CellValidator>>> key2cellValidators = new HashMap<>();

  /*==============
    error messages TODO
   ===============*/
  private List<DataValidateMessage> errorMessages = new ArrayList<>();

  private Workbook workbook;

  public DefaultExcelValidatorEngine(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public void addWorkbookValidator(WorkbookValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.workbookValidators, validators);
  }

  @Override
  public void addSheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return;
    }
    Collections.addAll(this.sheetValidators, validators);
  }

  @Override
  public void addRowValidator(RowValidator... validators) {
    if (validators == null) {
      return;
    }

    for (RowValidator validator : validators) {
      String key = validator.getKey();
      int sheetIndex = validator.getSheetIndex();

      if (!key2rowValidators.containsKey(sheetIndex)) {
        key2rowValidators.put(sheetIndex, new HashMap<>());
      }

      Map<String, List<RowValidator>> validatorsOfSheet = key2rowValidators.get(sheetIndex);
      if (!validatorsOfSheet.containsKey(key)) {
        validatorsOfSheet.put(key, new ArrayList<>());
      }
      validatorsOfSheet.get(key).add(validator);
    }
  }

  @Override
  public void addCellValidator(CellValidator... validators) {
    if (validators == null) {
      return;
    }
    for (CellValidator validator : validators) {
      String key = validator.getKey();
      int sheetIndex = validator.getSheetIndex();

      if (!key2cellValidators.containsKey(sheetIndex)) {
        key2cellValidators.put(sheetIndex, new HashMap<>());
      }

      Map<String, List<CellValidator>> validatorsOfSheet = key2cellValidators.get(sheetIndex);
      if (!validatorsOfSheet.containsKey(key)) {
        validatorsOfSheet.put(key, new ArrayList<>());
      }
      validatorsOfSheet.get(key).add(validator);
    }
  }

  @Override
  public boolean valid() {
    if (workbook == null) {
      throw new ExcelReadException("workbook is null");
    }

    // valid data
    validWorkbook(workbook);
    if (CollectionUtils.isNotEmpty(errorMessages)) {
      return false;
    }

    for (Sheet sheet : workbook.getSheets()) {

      // check dependency of this sheet
      checkValidatorKeyDependencyOfSheet(sheet.getIndex());

      validSheet(sheet);

      if (CollectionUtils.isNotEmpty(errorMessages)) {
        return false;
      }

      for (Row row : sheet.getRows()) {

        validRowCells(row, sheet.getIndex());
      }
    }

    return CollectionUtils.isEmpty(errorMessages);
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependencyOfSheet(int sheetIndex) {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(key2rowValidators.get(sheetIndex)));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(key2cellValidators.get(sheetIndex)));

    Set<String> satisfiedKeys = new HashSet<>();
    for (String key : dependsOnHierarchy.keySet()) {
      Set<String> dependencyKeys = new HashSet<>();
      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, key);
      satisfiedKeys.addAll(dependencyKeys);
    }
  }

  private void checkValidatorKeyDependencyHierarchy(
      Map<String, Set<String>> dependsOnHierarchy,
      Set<String> satisfiedKeys,
      Set<String> dependencyKeys,
      String key
  ) {

    if (satisfiedKeys.contains(key)) {
      return;
    }

    dependencyKeys.add(key);
    for (String dependsOn : dependsOnHierarchy.get(key)) {

      if (!dependsOnHierarchy.containsKey(dependsOn)) {
        throw new ExcelValidateException("dependency missing key [" + dependsOn + "]");
      }

      if (dependencyKeys.contains(dependsOn)) {
        throw new ExcelValidateException("dependency cycling on [" + key + "] and [" + dependsOn + "]");
      }

      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, dependsOn);
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validWorkbook(Workbook workbook) {

    for (WorkbookValidator validator : workbookValidators) {

      if (!validator.valid(workbook)) {

      }
    }

  }

  private void validSheet(Sheet sheet) {

    for (SheetValidator validator : sheetValidators) {

      if (!validator.valid(sheet)) {

      }
    }

  }

  private void validRowCells(Row row, int sheetIndex) {

    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    dependsOnHierarchy.putAll(buildDependsOnHierarchy(key2rowValidators.get(sheetIndex)));
    dependsOnHierarchy.putAll(buildDependsOnHierarchy(key2cellValidators.get(sheetIndex)));

    // one key corresponding multi validators
    Map<String, List<? extends DataValidator>> validatorMap = new HashMap<>();

    Map<String, List<RowValidator>> rowValidatorsOfSheet = key2rowValidators.get(sheetIndex);
    validatorMap.putAll(rowValidatorsOfSheet);
    Map<String, List<CellValidator>> cellValidatorsOfSheet = key2cellValidators.get(sheetIndex);
    validatorMap.putAll(cellValidatorsOfSheet);

    Map<String, Set<Boolean>> allResult = new HashMap<>();

    for (List<RowValidator> validators : rowValidatorsOfSheet.values()) {
      for (RowValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getKey()));
      }
    }
    for (List<CellValidator> validators : cellValidatorsOfSheet.values()) {
      for (CellValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getKey()));
      }
    }

  }

  private Map<String, Set<Boolean>> validRowCellsHierarchy(
      Map<String, List<? extends DataValidator>> validatorMap,
      Map<String, Set<Boolean>> allResult,
      Map<String, Set<String>> dependsOnHierarchy,
      Row row,
      String key
  ) {

    Map<String, Set<Boolean>> result = new HashMap<>();

    if (allResult.containsKey(key)) {
      result.put(key, allResult.get(key));
      return result;
    }

    Set<String> dependsOns = dependsOnHierarchy.get(key);

    if (CollectionUtils.isNotEmpty(dependsOns)) {

      for (String dependsOn : dependsOns) {

        result.putAll(validRowCellsHierarchy(validatorMap, result, dependsOnHierarchy, row, dependsOn));
      }

    }

    if (ifSkip(result)) {
      result.put(key, Collections.singleton(null));
      return result;
    }

    Set<Boolean> vrs = new HashSet<>();
    for (DataValidator dataValidator : validatorMap.get(key)) {
      vrs.add(doRowCellsValid(dataValidator, row));
    }
    result.put(key, vrs);
    return result;
  }

  private boolean ifSkip(Map<String, Set<Boolean>> result) {

    Set<Boolean> dependsOnVrs = new HashSet<>();
    for (Set<Boolean> vr : result.values()) {
      dependsOnVrs.addAll(vr);
    }

    return dependsOnVrs.contains(Boolean.FALSE) || dependsOnVrs.contains(null);
  }

  private boolean doRowCellsValid(DataValidator dataValidator, Row row) {

    if (dataValidator instanceof RowValidator) {

      return ((RowValidator) dataValidator).valid(row);
    } else {

      CellValidator cellValidator = (CellValidator) dataValidator;
      return cellValidator.valid(row.getCell(cellValidator.getMatchField()));
    }
  }

  private <VALIDATOR extends DataValidator> Map<String, Set<String>> buildDependsOnHierarchy(Map<String, List<VALIDATOR>> key2dataValidator) {
    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    for (Map.Entry<String, List<VALIDATOR>> entry : key2dataValidator.entrySet()) {
      String key = entry.getKey();
      dependsOnHierarchy.put(key, new HashSet<>());

      for (VALIDATOR dataValidator : entry.getValue()) {

        Set<String> dependsOn = dataValidator.getDependsOn();
        if (CollectionUtils.isNotEmpty(dependsOn)) {

          dependsOnHierarchy.get(key).addAll(dependsOn);
        }
      }
    }

    return dependsOnHierarchy;
  }
}
