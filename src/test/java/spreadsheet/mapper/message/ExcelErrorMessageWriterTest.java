package spreadsheet.mapper.message;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.message.ErrorMessage;
import spreadsheet.mapper.model.message.ErrorMessageBean;
import spreadsheet.mapper.model.message.MessageWriteStrategies;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Created by hanwen on 2017/1/6.
 */
public class ExcelErrorMessageWriterTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelErrorMessageWriterTest.class);

  private File file;

  @BeforeClass
  public void before() throws IOException {
    file = File.createTempFile("test", Constants.SUFFIX_XLSX);
    LOGGER.debug(file.getAbsolutePath());
  }

  @Test
  public void testWrite() throws Exception {

    ErrorMessageWriter messageWriter = new ExcelErrorMessageWriter(true);
    messageWriter.write(createErrorMessages(), new FileOutputStream(file));
  }

  private Collection<ErrorMessage> createErrorMessages() {

    List<ErrorMessage> errorMessages = new ArrayList<>();

    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, "test1", 1, 1, 1));
    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, "test2", 1, 1, 1));
    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, "test3", 1, 1, 1));
    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.COMMENT, "test4", 1, 3, 3));

    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, "test5", 1));
    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, "test6", 1));
    errorMessages.add(new ErrorMessageBean(MessageWriteStrategies.TEXT_BOX, "test7", 1));

    return errorMessages;

  }

}